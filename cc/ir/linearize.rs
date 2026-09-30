//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Converts the AST to SSA-form IR: basic blocks and typed pseudo-registers.
//

use super::mem2reg::mem2reg;
use super::ssa::ssa_convert;
use super::{
    BasicBlock, BasicBlockId, CallAbiInfo, Function, Initializer, Instruction, MemoryOrder, Module,
    Opcode, Pseudo, PseudoId, PseudoKind,
};
use crate::abi::{get_abi_for_conv, CallingConv};
use crate::diag::{get_all_stream_names, Position};
use crate::float::FloatVal;
use crate::ir::linearize_atomic::AtomicLvalue;
use crate::ir::linearize_emit::CompoundAssign;
use crate::parse::ast::{
    AssignOp, BinaryOp, BlockItem, Expr, ExprKind, ExternalDecl, FpCompare, FpTest, FunctionDef,
    GnuAtomicOp, InitElement, InlineLibraryFn, MemoryFn, NarrowedLibraryCall, OffsetOfPath,
    ParamStyle, TranslationUnit, UnaryOp,
};
use crate::strings::{StringId, StringTable};
use crate::symbol::{SymbolId, SymbolTable};
use crate::target::Target;
use crate::types::{MemberInfo, TypeId, TypeKind, TypeModifiers, TypeTable};
use std::collections::{HashMap, HashSet};

const DEFAULT_VAR_MAP_CAPACITY: usize = 64;
const DEFAULT_LABEL_MAP_CAPACITY: usize = 16;
const DEFAULT_LOOP_DEPTH_CAPACITY: usize = 4;
const DEFAULT_FILE_SCOPE_CAPACITY: usize = 16;

/// Which half of a complex value an rvalue read takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComplexHalf {
    Real,
    Imag,
}

/// One array extent of a variably-modified type.
///
/// A variably-modified type can mix constant and run-time extents
/// (`int a[n][3][m]`), so each level records which it is.
#[derive(Clone, Copy)]
pub(crate) enum VmDim {
    /// An extent known at compile time.
    Const(usize),
    /// An extent computed at run time and kept in a hidden local, so it
    /// survives SSA and can be reloaded at each use.
    Sym(PseudoId),
}

/// A scalar parameter, which the prologue stores into a local of its own.
struct ScalarParam {
    name: String,
    symbol: Option<SymbolId>,
    /// The declared type: the local's.
    typ: TypeId,
    /// The type the caller passes it as, which differs from `typ` only for
    /// an identifier-list definition ([`ParamStyle::IdentifierList`]).
    passed_as: TypeId,
    /// The incoming argument.
    arg: PseudoId,
}

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
    /// For a VLA: its outermost extent, the one `vm_row_dims` leaves out.
    /// `typeof(v)` names every extent of `v`'s type, this one included.
    pub(crate) vla_outer_extent: Option<VmDim>,
    /// Extents of this object's *element* type, outermost first: what one
    /// index step leaves behind. Empty unless the element type is variably
    /// modified.
    ///
    /// For a local `int b[n][m][k]` this is `[m, k]`; for a parameter
    /// `int a[n][m]` (adjusted to `int (*a)[m]`) and for the `int (*a)[m]`
    /// spelling alike it is `[m]`, which is why both index identically.
    /// Indexing at depth `d` needs `product(vm_row_dims[d..]) *
    /// sizeof(vla_elem_type)` -- a variably-modified type reports a
    /// compile-time size of 0, so without this every such stride would be 0.
    pub(crate) vm_row_dims: Vec<VmDim>,
    /// Where the object lives relative to this local's stack slot.
    pub(crate) storage: Storage,
}

/// Where a local's object lives relative to its stack slot.
#[derive(Clone, Copy)]
pub(crate) enum Storage {
    /// The slot *is* the object, so `&x` is the slot's address.
    InSlot,
    /// The slot holds a *pointer* to the object, which lives elsewhere: a
    /// VLA's `alloca`d storage, or the caller's `va_list`. `&x` is that
    /// pointer's value and has to be loaded; taking the slot's address
    /// yields a pointer to the pointer, which is what made `&vla` differ
    /// from the same array decayed.
    ///
    /// The [`TypeId`] is the type of the pointer *in the slot*. The two
    /// producers spell [`LocalVarInfo::typ`] differently -- a `va_list`
    /// parameter records the pointee there, a VLA records the pointer --
    /// so deriving it from `typ` cannot be right for both.
    Indirect(TypeId),
}

pub(crate) struct ResolvedDesignator {
    pub(crate) offset: usize,
    pub(crate) typ: TypeId,
    pub(crate) bit_offset: Option<u32>,
    pub(crate) bit_width: Option<u32>,
    pub(crate) access_bytes: Option<u32>,
    /// The member this designator chain named in each union it passed
    /// through. See [`UnionMembers`].
    pub(crate) unions: UnionMembers,
    /// The qualifiers `typ` inherits from what the chain passed through: the
    /// aggregates it named members of, and any anonymous ones the lookup
    /// crossed. See [`crate::types::TypeTable::subobject_type`].
    pub(crate) quals: TypeModifiers,
}

/// Which member a union came to hold, for each union an initializer list
/// reached.
///
/// C17 6.7.9p19 makes a later initializer override the earlier one for the
/// *same* subobject, and a union has only one subobject at a time: whether
/// `.u.p.y = 9` overrides part of what `.u = {1, 2}` wrote or replaces all of
/// it turns on whether the union still holds `p`. Neither the byte offset nor
/// the lowered [`Initializer`] can say -- every member of a union begins at
/// the same byte, and `Initializer` is what the emitter consumes and has no
/// room for a discriminant -- so the choice is recorded beside it.
///
/// Each entry is the byte offset of a union within the object whose
/// initializer list produced it, that union's type, and the index of the
/// member in question. The type is part of the key because a union declared
/// directly inside another begins at the same byte as it does.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct UnionMembers {
    members: Vec<(usize, TypeId, usize)>,
    /// Byte ranges a whole value initialized; see [`Held::Value`].
    values: Vec<std::ops::Range<usize>>,
}

/// What an initializer left a union holding.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Held {
    /// The member with this index, which an initializer list named.
    Member(usize),
    /// The bytes of a whole value -- `.u = v`, or `.s = t` for a struct `t`
    /// with a union inside -- which name no member.
    ///
    /// Which member `v` last had stored into it is a fact about the run, not
    /// the program, so no member can be said to be held; what is known is
    /// that every byte is `v`'s. A later designator reaching inside keeps
    /// them and replaces only what it names, whichever member it goes
    /// through, exactly as it would inside a struct initialized from a
    /// value. Treating the union as holding *nothing* instead reset it, so
    /// `{ .u = v, .u.s.b = 9 }` lost `v.s.a` while the same shape through a
    /// struct kept it.
    Value,
}

impl UnionMembers {
    /// Note that the union at `offset` holds `member`, replacing whatever it
    /// was last said to hold.
    pub(crate) fn record(&mut self, offset: usize, typ: TypeId, member: usize) {
        match self
            .members
            .iter_mut()
            .find(|e| (e.0, e.1) == (offset, typ))
        {
            Some(entry) => entry.2 = member,
            None => self.members.push((offset, typ, member)),
        }
    }

    /// Note that the bytes `range` hold a whole value, replacing whatever
    /// the unions inside them were last said to hold.
    pub(crate) fn record_value(&mut self, range: std::ops::Range<usize>) {
        self.members.retain(|e| !range.contains(&e.0));
        self.values.push(range);
    }

    /// What the union of type `typ` at `offset` holds, if anything says.
    fn held_at(&self, offset: usize, typ: TypeId) -> Option<Held> {
        if let Some(e) = self.members.iter().find(|e| (e.0, e.1) == (offset, typ)) {
            return Some(Held::Member(e.2));
        }
        self.values
            .iter()
            .any(|r| r.contains(&offset))
            .then_some(Held::Value)
    }

    /// Take on everything `other` says, which is later and so decisive.
    pub(crate) fn absorb(&mut self, other: &UnionMembers) {
        for r in &other.values {
            self.record_value(r.clone());
        }
        for &(offset, typ, member) in &other.members {
            self.record(offset, typ, member);
        }
    }

    /// Forget every union starting inside `range`, whose contents some later
    /// initializer has just discarded.
    pub(crate) fn clear_range(&mut self, range: std::ops::Range<usize>) {
        self.members.retain(|e| !range.contains(&e.0));
        // A value range the discarded bytes overlap no longer speaks for all
        // of itself; forgetting it only makes a later override reset rather
        // than keep, which is the answer that assumes nothing.
        self.values
            .retain(|r| r.end <= range.start || range.end <= r.start);
    }
}

/// What the two initializers being merged say about the unions between them,
/// and how far into the earlier one's object the merge has descended.
///
/// A union is descended into only where the two agree: `held` is the member
/// the earlier initializer gave a value to, `named` the member the later
/// one's designator reached through, and anything else means the union comes
/// to hold something different and everything it held goes. Both are keyed by
/// byte offset within the object whose initializer list holds both entries,
/// which is what `base` counts from.
#[derive(Clone, Copy, Default)]
pub(crate) struct UnionFold<'a> {
    held: Option<&'a UnionMembers>,
    named: Option<&'a UnionMembers>,
    base: usize,
}

impl<'a> UnionFold<'a> {
    pub(crate) fn new(held: &'a UnionMembers, named: &'a UnionMembers, base: usize) -> Self {
        Self {
            held: Some(held),
            named: Some(named),
            base,
        }
    }

    /// The same view, `offset` bytes further into the object.
    pub(crate) fn inside(self, offset: usize) -> Self {
        Self {
            base: self.base + offset,
            ..self
        }
    }

    /// The member both sides agree the union of type `typ` at the current
    /// offset holds, if they do.
    ///
    /// A union holding a whole value agrees with any member named: its bytes
    /// are all there to be kept. See [`Held::Value`].
    pub(crate) fn agreed(&self, typ: TypeId) -> Option<usize> {
        let Some(Held::Member(named)) = self.named?.held_at(self.base, typ) else {
            return None;
        };
        match self.held?.held_at(self.base, typ)? {
            Held::Value => Some(named),
            Held::Member(held) => (held == named).then_some(held),
        }
    }
}

pub(crate) struct RawFieldInit {
    pub(crate) offset: usize,
    pub(crate) field_size: usize,
    /// The type of the subobject this initializer names.
    ///
    /// Carried so that resolving two initializers that describe overlapping
    /// storage can ask *how* they overlap: a later one naming a member of an
    /// earlier one's struct or array replaces only that member, while one
    /// reachable only through a union replaces the union's whole contents.
    /// Byte spans alone cannot tell the two apart.
    pub(crate) typ: TypeId,
    pub(crate) init: Initializer,
    pub(crate) bit_offset: Option<u32>,
    pub(crate) bit_width: Option<u32>,
    /// The member each union inside this subobject came to hold.
    pub(crate) held: UnionMembers,
    /// The member each union this entry's designator passed through named.
    pub(crate) named: UnionMembers,
}

impl RawFieldInit {
    /// The bytes this field's initializer actually writes.
    ///
    /// For a bitfield that is narrower than its access window, which is what
    /// decides whether two initializers describe overlapping storage: the
    /// window of `unsigned a:1` after a `char` spans a byte the following
    /// `char` member owns, while the field itself does not.
    pub(crate) fn byte_span(&self) -> std::ops::Range<usize> {
        match (self.bit_offset, self.bit_width) {
            (Some(bit_offset), Some(bit_width)) => {
                let start = self.offset + (bit_offset / 8) as usize;
                let end = self.offset + (bit_offset + bit_width).div_ceil(8) as usize;
                start..end.max(start + 1)
            }
            _ => self.offset..self.offset + self.field_size,
        }
    }
}

/// How a byte range sits inside an object, as C17 6.7.9p19 needs to know it:
/// an initializer for a subobject overrides the previous initializer for
/// *that* subobject, and whether some other initializer survives depends on
/// what lies between the two.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SubobjectPlace {
    /// The range is a subobject reached through struct members and array
    /// elements only (possibly the whole object). Initializing it leaves
    /// every other subobject of the enclosing object untouched.
    Member,
    /// The range is reached only by descending into this union, whose bytes
    /// span `offset..offset + size` of the enclosing object. A union holds one
    /// member at a time, so initializing through it discards whatever the
    /// union held before.
    ThroughUnion { offset: usize, size: usize },
    /// The range is the storage of a bit-field declared by the struct
    /// spanning `offset..offset + size` of the enclosing object. A bit-field
    /// is not addressable storage of its own -- it shares a carrier with its
    /// neighbours -- so an initializer for one replaces its bits and leaves
    /// theirs, which is done in that struct's own initializer rather than by
    /// replacing a subobject.
    ///
    /// Only asked for, and only ever answered, for a bit-field's own bits.
    BitfieldCarrier { offset: usize, size: usize },
    /// The range is not a subobject at all: it straddles two members, or it is
    /// a bit-field carrier's window rather than a named object.
    NotASubobject,
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

/// Which ends of a block copy reach a `volatile` object. See
/// [`Linearizer::block_volatility`].
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct BlockVolatility {
    pub(crate) dst: bool,
    pub(crate) src: bool,
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
    pub(crate) access_bytes: Option<u32>,
    /// Index, in the member list walked, of the member this visit initializes
    /// or lies inside. For a union that is the member it comes to hold, which
    /// its byte offset cannot say.
    pub(crate) member_index: Option<usize>,
    /// The member this visit's designator chain named in each union it passed
    /// through, by byte offset within the object being initialized.
    pub(crate) unions: UnionMembers,
    /// The qualifiers `typ` inherits from inside the aggregate being walked --
    /// an anonymous `volatile` structure it lies in, or a qualified member a
    /// designator chain went through. Not those of the object itself, which
    /// the consumer applies. See [`crate::types::TypeTable::subobject_type`].
    pub(crate) quals: TypeModifiers,
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

// Linearizer

/// A declaration scope the linearizer has entered.
///
/// Handed out by [`Linearizer::push_scope`] and given back to
/// [`Linearizer::pop_scope`]. A block scope is also the lifetime of every
/// VLA declared in it (C17 6.2.4p7), so the token carries the [`VlaMark`]
/// depth as it stood on entry and leaving the scope puts the stack pointer
/// back to what the first mark above that depth captured.
///
/// Carrying both in one token is the point. The declaration scope and the
/// VLA scope used to be two stacks opened by hand at separate call sites,
/// and only some of the sites that opened the first opened the second: a VLA
/// declared in a `for` init clause, in the `for` arm of the switch-body
/// walker, or in a statement expression was never released. Now there is no
/// way to enter one without the other, and none to leave one without the
/// other.
#[must_use = "a scope that is entered must be left through pop_scope"]
pub(crate) struct Scope {
    /// The `vla_marks` depth on entry; every mark above it belongs to this
    /// scope and is released when it ends.
    pub(crate) vla_entry: usize,
}

/// A captured stack pointer and the loop/switch nesting it was captured at.
///
/// See [`Linearizer::vla_marks`].
pub(crate) struct VlaMark {
    pub(crate) mark: PseudoId,
    pub(crate) break_depth: usize,
    pub(crate) continue_depth: usize,
}

/// Where a call returning through the hidden pointer (sret) puts its result.
///
/// See [`Linearizer::hidden_return_slot`].
pub(crate) struct HiddenReturnSlot {
    /// The local the callee writes the value into.
    pub(crate) storage: PseudoId,
    /// Its address: the call's hidden first argument.
    pub(crate) arg: PseudoId,
    /// The argument's type, a pointer to the returned type.
    pub(crate) arg_typ: TypeId,
}

/// Where a jump whose VLA restore is still undecided may land.
pub(crate) enum GotoTarget {
    /// `goto L;`, or one label edge of an `asm goto`: a single block.
    Label(BasicBlockId),
    /// `goto *p;`. The address is not known here, so the jump is taken to
    /// reach any label whose address this function takes, and only the
    /// scopes that *every* candidate lies outside of may be released -- the
    /// **deepest** depth recorded for any of them. Releasing down to a
    /// shallower one would free storage still in scope at another candidate,
    /// which is the one error a missing restore cannot cause.
    AnyAddressTaken,
}

/// A forward `goto` whose VLA restore is decided once its label is placed.
///
/// `marks` is the mark stack as it stood at the jump, so the restore can name
/// whichever scope the label turns out to sit in: `marks[label_depth]` is the
/// stack pointer captured on entry to the outermost scope the jump leaves.
pub(crate) struct PendingGotoVla {
    /// Where the jump goes.
    pub(crate) target: GotoTarget,
    /// The block the branch was emitted into.
    pub(crate) bb: BasicBlockId,
    /// Where in that block the branch sits; the restore goes just before it.
    pub(crate) at: usize,
    /// The marks in force at the jump, outermost first.
    pub(crate) marks: Vec<PseudoId>,
}

/// Linearizer context for converting AST to IR
pub struct Linearizer<'a> {
    /// The module being built
    pub(crate) module: Module,
    /// Current function being linearized
    pub(crate) current_func: Option<Function>,
    /// Every CFG edge `link_bb`/`link_bb_many` have added to the current
    /// function. They are the only writers of `children` and `parents`
    /// during linearization, so this answers "is the edge already there?" in
    /// O(1) for both lists -- scanning `parents` instead made a label that
    /// thousands of `goto`s jump to quadratic in them.
    cfg_edges: HashSet<(BasicBlockId, BasicBlockId)>,
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
    /// The evaluated extents of each variably modified `typedef` in scope,
    /// keyed by the typedef's symbol.
    ///
    /// C17 6.7.7p3 evaluates them "each time the declaration of the typedef
    /// name is reached", once per execution of the typedef and not once per
    /// use, so they are recorded when the typedef is linearized and read back
    /// by every `ExprKind::VmTypedefExtent` that names it. Re-entering the
    /// declaration -- a typedef inside a loop -- overwrites the entry, which
    /// is exactly what "each time it is reached" asks for.
    pub(crate) vm_typedef_dims: HashMap<SymbolId, Vec<VmDim>>,
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
    /// Hidden return pointer (for functions returning via sret: large
    /// aggregates, and complex types the ABI classifies MEMORY)
    pub(crate) struct_return_ptr: Option<PseudoId>,
    /// The return type, when it is an aggregate the ABI returns in registers
    /// and wider than one. See [`Self::returns_reg_aggregate`].
    pub(crate) reg_aggregate_return_type: Option<TypeId>,
    /// Current function name (for generating unique static local names)
    pub(crate) current_func_name: String,

    /// Blocks whose address is taken by `&&label` in the function being
    /// linearized. Every indirect `goto` may reach any of them, so each is
    /// linked as a successor -- the CFG is explicit `children`/`parents`, and
    /// without the edges DCE would delete blocks nothing appears to reach.
    pub(crate) addr_taken_labels: Vec<BasicBlockId>,

    /// Every label this function names -- by `goto`, `&&label` or
    /// `asm goto` -- with where each was written. Checked against
    /// `defined_labels` once the body is walked, because a forward reference
    /// is legal and only the end of the function settles it.
    pub(crate) label_refs: Vec<(String, crate::diag::Position)>,

    /// Labels this function actually defines.
    pub(crate) defined_labels: std::collections::HashSet<String>,
    /// Every label written in this function's body, including one inside an
    /// operand that is never evaluated -- `sizeof(({ L: x; }))` -- and so
    /// never defined by lowering.
    pub(crate) written_labels: std::collections::HashSet<String>,
    /// How many VLA marks were in force at each label already linearized, for
    /// a function that declares a variable-length array.
    ///
    /// A VLA's storage lives until control leaves the scope of its
    /// declaration (C17 6.2.4p7), and a backward `goto` to a label ahead of
    /// that declaration leaves it. Without releasing the storage the stack
    /// grows every time round: `lab: int x[n]; ... goto lab;` died of stack
    /// exhaustion after a few thousand iterations. The mark at this index is
    /// the stack as it stood at the label, so the jump restores to it.
    ///
    /// Only labels in a function that declares a VLA appear here, so nothing
    /// is recorded for the ordinary case.
    ///
    /// Keyed by the label's block rather than its name: a computed `goto`
    /// knows its candidates only as the blocks in `addr_taken_labels`, and
    /// one map serves both it and the named jumps.
    pub(crate) label_vla_depth: std::collections::HashMap<BasicBlockId, usize>,
    /// Forward `goto`s that may be leaving a VLA's scope, to be resolved once
    /// every label's depth is known.
    ///
    /// A forward jump's target has not been linearized yet, so whether it is
    /// still inside the scope of the VLAs in force -- which C17 6.8.6.1p1
    /// allows, `{ char v[n]; if (x) goto done; done: use(v); }` -- or outside
    /// it cannot be decided at the jump. The two need different code, and one
    /// of them needs none, so the decision waits.
    pub(crate) pending_goto_vla: Vec<PendingGotoVla>,
    /// The stack pointer captured on entry to each open block that declares a
    /// VLA, with the `break_targets` and `continue_targets` depths at which
    /// it was captured.
    ///
    /// Leaving the block puts the pointer back, so a loop body's VLA is
    /// reclaimed each time round rather than growing the stack until the
    /// program dies. The recorded depths are what let `break` and `continue`
    /// unwind too: a mark taken at a depth at or beyond the current one was
    /// taken inside the construct being left, so restoring to the outermost
    /// such mark undoes exactly what that construct allocated -- without
    /// every loop having to push a parallel stack of its own.
    ///
    /// Both depths are needed because a `switch` pushes a break target and
    /// not a continue target. `continue` out of a switch leaves the whole
    /// loop body, including a VLA declared before the switch, and asking the
    /// break depth there found no mark to undo.
    pub(crate) vla_marks: Vec<VlaMark>,
    /// Whether this function declares anything variably modified, and so
    /// needs the bookkeeping above.
    pub(crate) func_has_vla: bool,

    /// The object an initializer is storing into, while the subobject being
    /// stored inherits `volatile` from something the member types cannot
    /// show.
    ///
    /// A member of a `volatile` object is volatile (C17 6.5.2.3p3), but the
    /// declared member types an initializer walks carry only what was written
    /// on each member, and this `TypeTable` is read-only here, so the
    /// so-qualified type cannot be interned. The walk names the object
    /// instead, and [`Self::mark_volatile_access`] -- the one place a marker
    /// is decided -- marks every access to it: the stores, and the carrier
    /// load a bit-field's read-modify-write performs. An access is only ever
    /// *added* a marker this way, and only one addressed through the named
    /// object: what an initializer expression reads is some other object and
    /// keeps its own answer.
    pub(crate) volatile_init_object: Option<PseudoId>,

    /// The one block every computed `goto` in this function branches through,
    /// and the hidden local carrying the target address to it.
    ///
    /// A `goto *p` can reach any address-taken label, and recording that
    /// directly gives one edge per (goto, label) pair -- N² for an interpreter
    /// dispatch loop, where every handler ends in a `goto *`. CPython's
    /// `ceval.c` has over two hundred of each, and the iterative dataflow over
    /// that CFG did not finish in twenty minutes. Funnelling every indirect
    /// branch through a single block makes it 2N: each `goto` has one
    /// successor, and only the dispatch block fans out.
    pub(crate) indirect_dispatch: Option<(BasicBlockId, PseudoId)>,
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
    /// Whether the current function is an *inline definition* -- one that
    /// provides no external definition, and is therefore the thing C99 6.7.4p3
    /// constrains.
    pub(crate) current_func_is_inline_definition: bool,
    /// Set of file-scope static variable names (for inline semantic checks)
    pub(crate) file_scope_statics: std::collections::HashSet<String>,
    /// Calling convention of the current function being linearized
    pub(crate) current_calling_conv: CallingConv,
    /// Scope stack for locals: each entry records (sym, previous_value) pairs
    /// for undoing inserts when a scope exits.
    pub(crate) local_scope_stack: Vec<Vec<(SymbolId, Option<LocalVarInfo>)>>,
    /// `__attribute__((alias))` declarations seen so far. Resolved once the
    /// whole unit has been read, because the target may be defined after the
    /// alias that names it.
    pub(crate) declared_aliases: Vec<super::linearize_init::DeclaredAlias>,
    /// Every function the translation unit defines, by name, weak ones aside:
    /// a call to a library builtin that yields to a definition reaches the
    /// program's own wherever it is (`InlineLibraryFn::yields_to_a_definition`).
    pub(crate) defined_functions: std::collections::HashSet<StringId>,
    /// `-ftrapping-math`, the default: the program may observe the
    /// floating-point exception flags, so a comparison that would raise one
    /// is emitted even where its answer is known. `-fno-trapping-math` turns
    /// it off.
    pub(crate) trapping_math: bool,
}

impl<'a> Linearizer<'a> {
    pub fn new(
        symbols: &'a SymbolTable,
        types: &'a TypeTable,
        strings: &'a StringTable,
        target: &'a Target,
    ) -> Self {
        Self {
            module: Module::default(),
            current_func: None,
            cfg_edges: HashSet::new(),
            current_bb: None,
            next_pseudo: 0,
            next_bb: 0,
            var_map: HashMap::with_capacity(DEFAULT_VAR_MAP_CAPACITY),
            locals: HashMap::with_capacity(DEFAULT_VAR_MAP_CAPACITY),
            vm_typedef_dims: HashMap::new(),
            label_map: HashMap::with_capacity(DEFAULT_LABEL_MAP_CAPACITY),
            break_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            continue_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            run_ssa: true, // Enable SSA conversion by default
            symbols,
            types,
            strings,
            struct_return_ptr: None,
            reg_aggregate_return_type: None,
            current_func_name: String::new(),
            addr_taken_labels: Vec::new(),
            label_refs: Vec::new(),
            defined_labels: std::collections::HashSet::new(),
            written_labels: std::collections::HashSet::new(),
            label_vla_depth: std::collections::HashMap::new(),
            pending_goto_vla: Vec::new(),
            vla_marks: Vec::new(),
            volatile_init_object: None,
            func_has_vla: false,
            indirect_dispatch: None,
            static_local_counter: 0,
            compound_literal_counter: 0,
            static_locals: HashMap::with_capacity(DEFAULT_LABEL_MAP_CAPACITY),
            current_pos: None,
            target,
            current_func_is_inline_definition: false,
            file_scope_statics: std::collections::HashSet::with_capacity(
                DEFAULT_FILE_SCOPE_CAPACITY,
            ),
            current_calling_conv: CallingConv::default(),
            local_scope_stack: Vec::new(),
            declared_aliases: Vec::new(),
            defined_functions: std::collections::HashSet::new(),
            trapping_math: true,
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

    /// Enter a declaration scope. Subsequent `insert_local` calls will record
    /// the previous value so `pop_scope` can restore it.
    ///
    /// Entering a declaration scope *is* entering a VLA scope: the returned
    /// [`Scope`] remembers the mark depth so `pop_scope` releases whatever
    /// the scope allocated. See [`Scope`] for why the two are one operation.
    pub(crate) fn push_scope(&mut self) -> Scope {
        self.local_scope_stack.push(Vec::new());
        Scope {
            vla_entry: self.vla_marks.len(),
        }
    }

    /// Leave the scope `scope` opened: release the VLAs declared in it and
    /// restore every local it shadowed.
    ///
    /// The stack restore comes first, while the block the scope ends in is
    /// still the current one, and is emitted only on the falling-out path --
    /// a `break`, `continue`, `goto` or `return` that left already did its
    /// own unwinding and terminated the block.
    pub(crate) fn pop_scope(&mut self, scope: Scope) {
        self.close_vla_scope(&scope);
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
    pub(crate) fn str(&self, id: StringId) -> &str {
        self.strings.get(id)
    }

    /// Get the name of a symbol as a String
    ///
    /// This is the name that reaches the assembler, so a GCC asm label
    /// (`extern int myfn(int) __asm__("realfn");`) wins over the declared
    /// identifier. Renaming here rather than in the backend means every
    /// consumer -- direct calls, global definitions, `extern_symbols`, GOT and
    /// TLS decisions, the macOS underscore -- sees one consistent name and
    /// needs no change of its own.
    /// A label is marked verbatim, because it *is* the assembler name and must
    /// not pick up the target's own decoration; see `lir::VERBATIM_MARKER`.
    pub(crate) fn symbol_name(&self, id: SymbolId) -> String {
        let sym = self.symbols.get(id);
        match &sym.asm_label {
            Some(label) => crate::arch::lir::verbatim(label),
            None => self.str(sym.name).to_string(),
        }
    }

    /// The emitted name for a declared identifier.
    ///
    /// A function *definition* reaches its name as a `StringId` rather than
    /// the `SymbolId` that carries the label, so it has to go back through the
    /// symbol table. Inner scopes are gone by the time linearization runs, so
    /// a file-scope function name resolves unambiguously.
    pub(crate) fn emitted_name(&self, name: StringId) -> String {
        self.symbols
            .lookup(name, crate::symbol::Namespace::Ordinary)
            .and_then(|s| s.asm_label.as_deref())
            .map(crate::arch::lir::verbatim)
            .unwrap_or_else(|| self.str(name).to_string())
    }

    /// The assembler name of the C library function `name`, for a call the
    /// compiler emits itself -- a `__builtin_memcpy`, a structure copy, a
    /// zero fill, `setjmp`.
    ///
    /// Such a call is still a call to *that function*, so a program that
    /// declared it with an asm label (`void *memcpy(...) __asm("my_memcpy")`,
    /// or glibc's fortified `longjmp` -> `__longjmp_chk`) gets the label, as
    /// gcc gives it. Every instruction the backends lower to a library call
    /// takes its callee from here, never from a literal in the backend.
    pub(crate) fn library_function_name(&self, name: &str) -> String {
        match self.strings.lookup(name) {
            Some(id) => self.emitted_name(id),
            None => name.to_string(),
        }
    }

    /// Whether the C library function `name` is one a fold may call: this
    /// unit has not bound the name to something that is not a function.
    ///
    /// A program may write `int puts;`, redefining a name C17 7.1.3 reserves
    /// to the implementation. That makes the program undefined, but gcc and
    /// clang degrade gracefully -- they decline the rewrite and keep the call
    /// the program wrote. Taking the name regardless emits `call puts`
    /// against the object's own storage, which crashes.
    ///
    /// A name this unit never mentions is available: the linker supplies it.
    fn library_function_available(&self, name: &str) -> bool {
        let Some(id) = self.strings.lookup(name) else {
            return true;
        };
        match self.symbols.lookup(id, crate::symbol::Namespace::Ordinary) {
            Some(sym) => self.types.kind(sym.typ) == TypeKind::Function,
            None => true,
        }
    }

    /// Whether any declaration of `name` in this translation unit said
    /// `extern`. See [`crate::symbol::Symbol::has_extern_decl`].
    pub(crate) fn has_extern_decl(&self, name: StringId) -> bool {
        self.symbols
            .lookup(name, crate::symbol::Namespace::Ordinary)
            .is_some_and(|s| s.has_extern_decl)
    }

    /// Whether any declaration of `name` omitted `inline`.
    /// See [`crate::symbol::Symbol::has_non_inline_decl`].
    pub(crate) fn has_non_inline_decl(&self, name: StringId) -> bool {
        self.symbols
            .lookup(name, crate::symbol::Namespace::Ordinary)
            .is_some_and(|s| s.has_non_inline_decl)
    }

    pub fn linearize(&mut self, tu: &TranslationUnit) -> Module {
        self.defined_functions = tu
            .items
            .iter()
            .filter_map(|item| match item {
                // A weak definition may be replaced at link time, and gcc
                // leaves the builtin in its place.
                ExternalDecl::FunctionDef(func) if !func.attrs.symbol.weak => Some(func.name),
                _ => None,
            })
            .collect();
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
        self.resolve_aliases();
        for &name in super::FOLD_CALLEES {
            if !self.library_function_available(name) {
                continue;
            }
            let symbol = self.library_function_name(name);
            self.module.library_symbols.insert(name, symbol);
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
    ///
    /// The 16-byte arm carries `__int128` bit-fields wider than 64 bits. It
    /// must name a type whose kind is `Int128`, because that is what routes
    /// the pseudos to 16-byte stack slots in the register allocator -- a
    /// 128-bit value the allocator hands a single GP register instead panics
    /// the backend in `int128_lo_mem_loc`.
    /// `storage_size` is a byte count, so it takes the type an object size is
    /// counted in. Only 1, 2, 4, 8 and 16 name a storage unit; anything else
    /// -- including a size no integer type could hold -- takes the default.
    pub(crate) fn bitfield_storage_type(&self, storage_size: usize) -> TypeId {
        match storage_size {
            1 => self.types.uchar_id,
            2 => self.types.ushort_id,
            4 => self.types.uint_id,
            8 => self.types.ulong_id,
            16 => self.types.uint128_id,
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
        let insn = self.mark_volatile_access(insn);
        let insn = self.displacement_in_range(insn);
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

    /// Mark an access to a `volatile` object as one, from the type it reaches.
    ///
    /// Every `Load` and `Store` the linearizer emits passes through
    /// [`Self::emit`], and each carries in `typ` the type of the object it is
    /// accessing -- so this is the one place the qualifier has to be read, and
    /// the one place it can be read for *every* access, including `*p` for a
    /// `volatile int *p`, where there is no variable holding the qualifier to
    /// ask (which is why `LocalVar::is_volatile` alone let DCE delete every
    /// discarded `volatile` read from `-O1` up).
    ///
    /// A marker a site set itself is kept rather than recomputed, so a site
    /// that knows more than the access type does can say so: a bit-field reads
    /// a storage unit whose type is the carrier, and a composite copy reads
    /// integer chunks, neither of which is the qualified type.
    ///
    /// An access addressed through [`Self::volatile_init_object`] is marked
    /// whatever its type says, which is how an initializer's accesses to the
    /// members of a `volatile` object are marked.
    fn mark_volatile_access(&self, mut insn: Instruction) -> Instruction {
        if !matches!(insn.op, Opcode::Load | Opcode::Store) || insn.is_volatile {
            return insn;
        }
        if self.volatile_init_object.is_some()
            && insn.src.first().copied() == self.volatile_init_object
        {
            insn.is_volatile = true;
            return insn;
        }
        if let Some(typ) = insn.typ {
            insn.is_volatile = self.types.contains_volatile(typ);
        }
        insn
    }

    /// Keep a load's or store's constant offset inside a machine displacement.
    ///
    /// Both backends address `src[0] + offset` with a signed 32-bit
    /// displacement, so an offset past `i32` -- a member more than 2 GiB into a
    /// struct, reached through a pointer -- used to be truncated with `as i32`
    /// and the access landed gigabytes away. This is the one place such an
    /// offset can enter the IR: no pass rewrites an offset afterwards. The far
    /// part is folded into the address with an ordinary `Add`, so the register
    /// allocator supplies the register it needs, and
    /// [`Instruction::displacement`] can rely on the result
    /// (`validate.rs` I6 checks it).
    fn displacement_in_range(&mut self, mut insn: Instruction) -> Instruction {
        if !matches!(insn.op, Opcode::Load | Opcode::Store) || i32::try_from(insn.offset).is_ok() {
            return insn;
        }
        let Some(&base) = insn.src.first() else {
            return insn;
        };
        let ptr = self.types.pointer_to(self.types.char_id);
        let base = self.rvalue_addr(base, self.types.char_id);
        let delta = self.emit_const(insn.offset as i128, self.types.long_id);
        let addr = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(Opcode::Add, addr, base, delta, ptr, 64));
        insn.src[0] = addr;
        insn.offset = 0;
        insn
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

            // The operands are compared at their own width: sized by the
            // `_Bool` result, `(_Bool)0x100000000L` compared only the low
            // half and came out 0.
            self.emit(Instruction::compare(
                opcode,
                result,
                (val, zero),
                (from_typ, from_size),
                (to_typ, to_size),
            ));

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

    /// Link `from` to each of `targets`, in order, as `link_bb` would one at
    /// a time.
    pub(crate) fn link_bb_many(
        &mut self,
        from: BasicBlockId,
        targets: impl IntoIterator<Item = BasicBlockId>,
    ) {
        for to in targets {
            self.link_bb(from, to);
        }
    }

    /// Link two basic blocks (parent -> child), once: a second link of the
    /// same edge adds nothing.
    pub(crate) fn link_bb(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if !self.cfg_edges.insert((from, to)) {
            return;
        }
        let func = self.current_func.as_mut().unwrap();
        if let Some(from_bb) = func.get_block_mut(from) {
            from_bb.children.push(to);
        }
        if func.get_block(to).is_none() {
            func.add_block(BasicBlock::new(to));
        }
        if let Some(to_bb) = func.get_block_mut(to) {
            to_bb.parents.push(from);
        }
    }

    /// Switch to a new basic block
    pub(crate) fn switch_bb(&mut self, id: BasicBlockId) {
        self.current_bb = Some(id);
        self.get_or_create_bb(id);
    }

    /// Continue in a fresh block that nothing branches to, after a `return`,
    /// `break` or `continue`.
    ///
    /// What follows is unreachable until a label, and still gets a block of
    /// its own so every construct in it is lowered the ordinary way; the
    /// block is removed when the function is finished
    /// (`dce::remove_unreachable_blocks`). Appending it to the block the
    /// jump ends instead put instructions after a terminator.
    pub(crate) fn start_unreachable_block(&mut self) {
        let bb = self.alloc_bb();
        self.switch_bb(bb);
    }

    /// Emit `insn`, a terminator control never comes back from -- `longjmp`,
    /// `__builtin_unreachable`, the `Unreachable` after a `noreturn` call --
    /// and carry on in a fresh block no edge reaches.
    ///
    /// What the source says next is dead, but it is still lowered, and it has
    /// to go somewhere that is not after a terminator: `longjmp` and
    /// `__builtin_unreachable` left it in the same block, which the verifier
    /// rejects and each back end would have emitted after the jump.
    pub(crate) fn emit_no_return(&mut self, insn: Instruction) {
        debug_assert!(insn.op.is_terminator());
        self.emit(insn);
        self.start_unreachable_block();
    }

    /// The block to emit into, starting an unreachable one where there is
    /// none.
    ///
    /// `current_bb` is `None` wherever control cannot arrive -- after a
    /// `goto`, and before a `switch`'s first `case` -- and [`Self::emit`]
    /// quietly drops what it is handed there. A construct that builds
    /// *control flow* of its own cannot be dropped that way: it has to hang
    /// its blocks off an existing one. This is that block, and
    /// `dce::remove_unreachable_blocks` takes it away again with everything
    /// lowered into it.
    pub(crate) fn current_or_unreachable_bb(&mut self) -> BasicBlockId {
        if self.current_bb.is_none() {
            self.start_unreachable_block();
        }
        self.current_bb
            .expect("start_unreachable_block leaves a current block")
    }

    // Function linearization

    /// Whether a return value of this type comes back through a hidden
    /// pointer (sret) rather than in registers.
    ///
    /// A complex value does exactly when the ABI classifies its return
    /// MEMORY, whatever its size. So `_Complex __int128` and, on x86-64,
    /// `_Float128 _Complex` -- both thirty-two bytes -- come back through the
    /// hidden pointer, while `long double _Complex`, as large, does not:
    /// System V classifies it COMPLEX_X87 and returns it in st(0)/st(1).
    /// libgcc's `__mul?c3`/`__div?c3` follow the same classification, which
    /// is why the complex arithmetic asks this too
    /// (`emit_complex_rtlib_call`).
    pub(crate) fn returns_via_hidden_pointer(&self, typ: TypeId) -> bool {
        let kind = self.types.kind(typ);
        if self.types.is_complex(typ) {
            let abi = get_abi_for_conv(self.current_calling_conv, self.target);
            return matches!(
                abi.classify_return(typ, self.types),
                crate::abi::ArgClass::Indirect { .. }
            );
        }
        if kind != TypeKind::Struct && kind != TypeKind::Union {
            // Scalars never do.
            return false;
        }
        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
        let class = abi.classify_return(typ, self.types);
        if matches!(class, crate::abi::ArgClass::Hfa { .. }) {
            // An HFA comes back in one V register per element, up to four, so
            // size does not enter into it: `struct { double a, b, c, d; }` is
            // thirty-two bytes and still returns in V0-V3, never through the
            // size rule below.
            return false;
        }
        if self.types.size_bits(typ) > self.target.max_aggregate_register_bits {
            // Every other aggregate over two eightbytes.
            return true;
        }
        // Two eightbytes or fewer: only the classifier knows. An aggregate
        // this small can still be MEMORY class -- `union { long double v;
        // double d; }` merges X87 with SSE, which is MEMORY, and gcc returns
        // it through a hidden pointer.
        matches!(class, crate::abi::ArgClass::Indirect { .. })
    }

    /// va_list parameters arrive as pointers -- array decay happened at the
    /// call site -- so what goes to the local is the pointer value.
    fn store_valist_params(
        &mut self,
        valist_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)>,
    ) {
        // Handle va_list parameters: store the pointer value (not the struct)
        for (name, symbol_id_opt, typ, arg_pseudo) in valist_params {
            // va_list params are passed as pointers due to array decay at call site.
            // Store the pointer value (8 bytes) to a local.
            let ptr_type = self.types.pointer_to(typ);
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                func.add_local(&name, local_sym, ptr_type, None, None);
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
                        vla_outer_extent: None,
                        vla_elem_type: None,
                        vm_row_dims: vec![],
                        // va_list param: the slot holds a pointer to the
                        // caller's va_list, array decay having happened at
                        // the call site.
                        storage: Storage::Indirect(ptr_type),
                    },
                );
            }
        }
    }

    /// Copy struct parameters into local storage so member access works.
    fn store_struct_params(
        &mut self,
        struct_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)>,
    ) {
        // Copy struct parameters to local storage so member access works
        for (name, symbol_id_opt, typ, arg_pseudo) in struct_params {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                func.add_local(&name, local_sym, typ, None, None);
            }

            let typ_size = self.types.size_bits(typ);
            // The copy length is an object size, so it is counted in bytes.
            // `typ_size` saturates for an aggregate past `u32::MAX` bits and is
            // good only for the class tests below, which compare against 64.
            let typ_bytes = self.types.size_bytes(typ) as i64;
            let is_aarch64 = self.target.arch == crate::target::Arch::Aarch64;
            // MEMORY class: the caller left the bytes in the incoming argument
            // area, so `arg_pseudo` names storage rather than pointing at it.
            // Over sixteen bytes that is every aggregate; at or below, only one
            // whose eightbyte holds a `long double`. On aarch64 every struct
            // parameter still arrives as a pointer, so the deref path below is
            // the right one there.
            // Exactly the test the caller uses when it decides to push the
            // bytes, so the two cannot drift: `long double _Complex` is
            // COMPLEX_X87 and travels in memory too, and asking only about
            // struct kinds sent it down the pointer path the caller had not
            // taken.
            let arrived_by_value =
                !is_aarch64 && crate::arch::lir::memory_class_bytes(self.types, typ).is_some();
            if arrived_by_value {
                // Passed by value on the stack. `arg_pseudo` is an IncomingArg
                // naming the struct data; take its address, then copy each
                // 8-byte chunk.
                let ptr_type = self.types.pointer_to(typ);
                let addr_pseudo = self.alloc_reg_pseudo();
                self.emit(Instruction::sym_addr(addr_pseudo, arg_pseudo, ptr_type));

                // Through the shared block copy, which knows two things this
                // loop did not: a copy past 128 bytes becomes a `memcpy` call
                // rather than an unbounded unroll, and a size that is not a
                // multiple of eight is copied exactly. Stepping 8 while
                // `offset < size` rounds *up* -- a 12-byte struct wrote 16
                // bytes, four of them past the local.
                let vol = BlockVolatility {
                    dst: self.types.contains_volatile(typ),
                    src: false,
                };
                self.emit_block_copy(local_sym, addr_pseudo, typ_bytes, vol);
            } else if typ_size > 64 {
                // Medium struct (9-16 bytes): arg_pseudo is a pointer (current behavior).
                // Copy each 8-byte chunk through pointer dereference.
                let vol = BlockVolatility {
                    dst: self.types.contains_volatile(typ),
                    src: false,
                };
                self.emit_block_copy(local_sym, arg_pseudo, typ_bytes, vol);
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
                        vla_outer_extent: None,
                        vla_elem_type: None,
                        vm_row_dims: vec![],
                        storage: Storage::InSlot,
                    },
                );
            }
        }
    }

    /// Complex parameters arrive in FP registers and the backend prologue
    /// stores them, so no store is emitted here -- only the local, and the
    /// `implicit_param_copies` record the inliner needs to do the same job
    /// when there is no prologue to run.
    fn store_complex_params(
        &mut self,
        complex_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId, u32)>,
    ) {
        // Setup local storage for complex parameters
        // Complex types are passed in FP registers per ABI - the prologue codegen
        // handles storing from XMM registers to local storage
        for (name, symbol_id_opt, typ, _arg_pseudo, arg_idx) in complex_params {
            let is_complex = self.types.is_complex(typ);
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            let typ_size_bytes = self.types.size_bytes(typ);
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                func.add_local(&name, local_sym, typ, None, None);
                // Record for inliner: the backend prologue fills this local from
                // registers; the inliner must generate an explicit copy instead.
                func.implicit_param_copies.push(super::ImplicitParamCopy {
                    arg_index: arg_idx,
                    local_sym,
                    size_bytes: typ_size_bytes,
                    qword_type: self.types.long_id,
                    // `complex_params` carries two kinds: a genuine `_Complex`,
                    // which travels by address at every size, and a struct the
                    // ABI puts in two FP registers, which travels by value when
                    // it fits in one. Only the type can tell them apart.
                    arg_is_address: is_complex,
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
                        vla_outer_extent: None,
                        vla_elem_type: None,
                        vm_row_dims: vec![],
                        storage: Storage::InSlot,
                    },
                );
            }
        }
    }

    /// Store scalar parameters into local storage, so that a parameter
    /// reassigned inside a branch still gets its phi nodes at the merge.
    fn store_scalar_params(&mut self, scalar_params: Vec<ScalarParam>) {
        // Store scalar parameters to local storage for SSA-correct reassignment handling
        // This ensures that if a parameter is reassigned inside a branch, phi nodes
        // are properly inserted at merge points.
        for ScalarParam {
            name,
            symbol: symbol_id_opt,
            typ,
            passed_as,
            arg,
        } in scalar_params
        {
            // Create a symbol pseudo for this local variable (its address)
            let local_sym = self.alloc_pseudo();
            let sym = Pseudo::sym(local_sym, name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(sym);
                func.add_local(&name, local_sym, typ, None, None);
            }

            // Store the incoming argument value to the local, converted from
            // its promoted type when an identifier list declared it.
            let arg_pseudo = if passed_as == typ {
                arg
            } else {
                self.emit_convert(arg, passed_as, typ)
            };
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
                        vla_outer_extent: None,
                        vla_elem_type: None,
                        vm_row_dims: vec![],
                        storage: Storage::InSlot,
                    },
                );
            }
        }
    }

    /// Clear the per-function state carried on the linearizer.
    ///
    /// `static_locals` is deliberately not cleared: it persists across
    /// functions.
    ///
    /// Returns the function-level [`Scope`], which `linearize_function` gives
    /// back once the body is lowered. Nothing is released there -- the
    /// epilogue restores `%rsp` from the frame pointer, and the body's own
    /// block scope has already dropped every mark -- but it is entered the
    /// same way as any other scope so that no site can enter one without the
    /// other.
    fn reset_for_function(&mut self, func: &FunctionDef) -> Scope {
        // Reset per-function state
        self.next_pseudo = 0;
        self.next_bb = 0;
        self.var_map.clear();
        self.locals.clear();
        self.local_scope_stack.clear();
        self.label_map.clear();
        self.break_targets.clear();
        self.continue_targets.clear();
        self.struct_return_ptr = None;
        self.reg_aggregate_return_type = None;
        self.current_func_name = self.emitted_name(func.name);
        self.addr_taken_labels.clear();
        self.label_refs.clear();
        self.defined_labels.clear();
        self.label_vla_depth.clear();
        self.pending_goto_vla.clear();
        self.vla_marks.clear();
        self.func_has_vla = Self::declares_vla(&func.body);
        self.indirect_dispatch = None;
        // Remove from extern_symbols since we're defining this function
        self.module.extern_symbols.remove(&self.current_func_name);
        // Note: static_locals is NOT cleared - it persists across functions

        // After `vla_marks.clear()`: the scope records the depth it starts
        // at, which for the function scope has to be zero.
        self.push_scope()
    }

    /// Whether a function body declares anything variably modified.
    ///
    /// `vla_sizes` on a declarator is the marker the jump checker already
    /// uses: it is non-empty for the array itself and for a pointer to one,
    /// both of which C17 6.7.6.2 calls variably modified. A pointer allocates
    /// nothing, so this over-approximates -- and the cost of a false positive
    /// is one register capture per label in that function, which is why the
    /// cheap test is the right one.
    fn declares_vla(stmt: &crate::parse::ast::Stmt) -> bool {
        match stmt {
            crate::parse::ast::Stmt::Block(items) => items.iter().any(|item| match item {
                crate::parse::ast::BlockItem::Declaration(decl) => {
                    decl.declarators.iter().any(|d| !d.vla_sizes.is_empty())
                }
                crate::parse::ast::BlockItem::Statement(s) => Self::declares_vla(s),
            }),
            crate::parse::ast::Stmt::If {
                then_stmt,
                else_stmt,
                ..
            } => {
                Self::declares_vla(then_stmt)
                    || else_stmt.as_ref().is_some_and(|e| Self::declares_vla(e))
            }
            crate::parse::ast::Stmt::While { body, .. }
            | crate::parse::ast::Stmt::DoWhile { body, .. }
            | crate::parse::ast::Stmt::Switch { body, .. }
            | crate::parse::ast::Stmt::Label { stmt: body, .. }
            | crate::parse::ast::Stmt::Case(_, _, body)
            | crate::parse::ast::Stmt::Default(_, body) => Self::declares_vla(body),
            crate::parse::ast::Stmt::For { init, body, .. } => {
                init.as_ref().is_some_and(|i| match i {
                    crate::parse::ast::ForInit::Declaration(decl) => {
                        decl.declarators.iter().any(|d| !d.vla_sizes.is_empty())
                    }
                    crate::parse::ast::ForInit::Expression(_) => false,
                }) || Self::declares_vla(body)
            }
            _ => false,
        }
    }

    pub(crate) fn linearize_function(&mut self, func: &FunctionDef) {
        // Set current position for debug info (function definition location)
        self.current_pos = Some(func.pos);

        // C17 6.8.6.1p1, before anything is lowered: entering the scope of a
        // variably modified identifier without executing its declaration
        // leaves the object's size never computed. gcc holds a statement
        // expression to the same rule.
        let written_labels = self.check_jumps_into_protected_scopes(&func.body);

        let func_scope = self.reset_for_function(func);
        self.written_labels = written_labels;

        // Create function - use storage class from FunctionDef
        let modifiers = self.types.modifiers(func.return_type);
        let is_static = func.is_static;
        let is_inline = func.is_inline;
        let is_extern = modifiers.contains(TypeModifiers::EXTERN);
        let is_noreturn = modifiers.contains(TypeModifiers::NORETURN);

        // Store calling convention from function attributes (e.g., __attribute__((sysv_abi)))
        self.current_calling_conv = func.calling_conv;

        let mut ir_func = Function::new(self.emitted_name(func.name), func.return_type);

        // Whether this is an *inline definition*, which provides no external
        // definition and so must not be emitted.
        //
        // C99 6.7.4p6: it is one if every file-scope declaration includes
        // `inline` and none includes `extern`. Both halves matter, and both
        // are whole-translation-unit questions rather than properties of this
        // definition's own tokens -- the declaration that settles it is
        // allowed to come afterwards, and in the standard idiom it does. They
        // are therefore read back from the symbol.
        //
        // GNU inline, selected by `__gnu_inline__`, is the exact opposite on
        // the `extern` question: there `extern inline` is the one that
        // provides no external definition. glibc's `__fortify_function` relies
        // on it.
        //
        // `static inline` is neither -- it has internal linkage and is emitted
        // like any other static function.
        let has_extern_decl = is_extern || self.has_extern_decl(func.name);
        let all_decls_inline = !self.has_non_inline_decl(func.name);
        // `-fgnu89-inline` makes the GNU rule the default for every inline
        // function, which is what the attribute selects one at a time.
        let gnu_inline = func.attrs.gnu_inline || crate::builtins::gnu89_inline();
        let is_inline_definition = is_inline
            && !is_static
            && if gnu_inline {
                has_extern_decl
            } else {
                !has_extern_decl && all_decls_inline
            };

        // C99 6.7.4p3 constrains an inline *definition*, not every non-static
        // inline function: what it forbids -- naming an identifier with
        // internal linkage, defining a modifiable static object -- would make
        // the several inline definitions of a function differ from each other
        // and from the external one. A definition that *is* the external
        // definition, because some declaration of it says `extern`, is an
        // ordinary function and may name whatever any function may name.
        self.current_func_is_inline_definition = is_inline_definition;

        ir_func.is_static = is_static;
        ir_func.emit = !is_inline_definition;
        ir_func.is_noreturn = is_noreturn;
        ir_func.is_inline = is_inline;
        ir_func.symbol_attrs = func.attrs.symbol.clone();
        // `alias` on a definition -- written on it, or on an earlier
        // prototype -- asks for two things one symbol cannot be. Recorded
        // like any other alias, so `resolve_aliases` reports it once.
        if ir_func.symbol_attrs.alias.take().is_some() {
            let kind = super::linearize_init::AliasKind::Function;
            self.declare_alias(&ir_func.name, &func.attrs.symbol, is_static, kind, func.pos);
        }
        ir_func.align = func.attrs.align;
        ir_func.is_noinline = func.attrs.noinline;
        ir_func.declared_effect = func.attrs.effect;
        ir_func.is_always_inline = func.attrs.always_inline;
        ir_func.constructor = func.attrs.constructor;
        ir_func.destructor = func.attrs.destructor;

        let ret_kind = self.types.kind(func.return_type);
        // Check if function returns a large struct
        // Large structs are returned via a hidden first parameter (sret)
        // that points to caller-allocated space
        let returns_large_struct = self.returns_via_hidden_pointer(func.return_type);

        // Argument index offset: if returning large struct, first arg is hidden return pointer
        let arg_offset: u32 = if returns_large_struct { 1 } else { 0 };

        // Add hidden return pointer parameter if needed
        if returns_large_struct {
            let sret_id = self.alloc_pseudo();
            let sret_pseudo = Pseudo::arg(sret_id, 0).with_name("__sret");
            ir_func.add_pseudo(sret_pseudo);
            self.struct_return_ptr = Some(sret_id);
        }

        if self.returns_reg_aggregate(func.return_type) {
            self.reg_aggregate_return_type = Some(func.return_type);
        }

        // A complex value comes back as the address of its halves, which the
        // inliner cannot splice into a caller expecting the value. An
        // aggregate returned by address -- one SSE register, x87, an HFA --
        // is not refused: every such `Ret` carries its ABI classification
        // (`emit_reg_aggregate_return`), and the inliner copies the bytes it
        // names into the call's result local, which is where a call leaves
        // them.
        ir_func.ret_is_address = self.types.is_complex(func.return_type);

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
        let mut scalar_params: Vec<ScalarParam> = Vec::with_capacity(func.params.len());
        // va_list parameters need special handling (pointer storage)
        let mut valist_params: Vec<(String, Option<SymbolId>, TypeId, PseudoId)> =
            Vec::with_capacity(func.params.len());

        for (i, param) in func.params.iter().enumerate() {
            let name = param
                .symbol
                .map(|id| self.symbol_name(id))
                .unwrap_or_else(|| format!("arg{}", i));
            // What the caller passes: the parameter's own type under a
            // prototype, its default argument promotion under an identifier
            // list -- converted back to the declared type on entry.
            let passed_as = match func.param_style {
                ParamStyle::Prototype => param.typ,
                ParamStyle::IdentifierList => self.types.default_argument_promote(param.typ),
            };
            ir_func.add_param(&name, passed_as);

            // Create argument pseudo (offset by 1 if there's a hidden return pointer)
            let pseudo_id = self.alloc_pseudo();
            let pseudo = Pseudo::arg(pseudo_id, i as u32 + arg_offset).with_name(&name);
            ir_func.add_pseudo(pseudo);

            // For struct/union types, we'll copy to a local later
            // so member access works properly
            let param_kind = self.types.kind(param.typ);
            if param_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
                // va_list parameters are special: due to array-to-pointer decay at call site,
                // the actual value passed is a pointer to the va_list struct, not the struct itself.
                // We'll handle this after function setup.
                //
                // Only where `va_list` is an array type. Where it is already a
                // pointer -- Apple aarch64 -- nothing decays: the parameter
                // *is* the va_list object, so it takes the ordinary scalar
                // path and gets a slot of its own. `va_arg` needs that slot's
                // address to advance it; handing it the pointer value instead
                // made the first `va_arg` dereference the first variadic
                // argument as though it were an address.
                valist_params.push((name, param.symbol, param.typ, pseudo_id));
            } else if param_kind == TypeKind::Struct || param_kind == TypeKind::Union {
                // Medium structs (9-16 bytes) with all-SSE classification are
                // passed like complex types (in two XMM registers). Route them
                // through complex_params so the codegen handles the register split.
                let size = self.types.size_bits(param.typ);
                // An HFA arrives in one register per element whatever its
                // size, and the prologue writes them into the local -- so it
                // must not also get the small-struct store below, which would
                // overwrite what the prologue just put there.
                let is_hfa_param = {
                    let abi = get_abi_for_conv(self.current_calling_conv, self.target);
                    matches!(
                        abi.classify_param(param.typ, self.types),
                        crate::abi::ArgClass::Hfa { count, .. }
                            if count >= 2 || size > 64
                    )
                };
                let is_two_fp_regs = is_hfa_param
                    || size > 64 && size <= 128 && {
                        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
                        let class = abi.classify_param(param.typ, self.types);
                        // Any all-SSE aggregate, whether that is two registers of
                        // eight bytes or one of sixteen.
                        let all_sse = matches!(
                            class,
                            crate::abi::ArgClass::Direct { ref classes, .. }
                                if !classes.is_empty()
                                    && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                        ) || matches!(class, crate::abi::ArgClass::Hfa { .. });
                        // Two eightbytes of any classes -- both integer, or one
                        // of each -- arrive in two registers on both targets:
                        // AAPCS64 §5.4.2 C.10 and SysV AMD64 §3.2.3 agree.
                        let reg_pair = matches!(
                            class,
                            crate::abi::ArgClass::Direct { ref classes, .. }
                                if classes.len() == 2
                        );
                        all_sse || reg_pair
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
                // Does this complex parameter arrive by address or in
                // registers? Ask the *target's* ABI: on x86_64 only
                // `long double _Complex` is MEMORY class, but on Apple
                // aarch64 `long double` is a plain double, so all three are
                // ordinary two-register HFAs there.
                //
                // Getting it wrong is not a missing copy but a duplicated
                // one: the address path emits a load from the incoming
                // pointer while the prologue also stores the argument
                // registers into the same local, and the load wins — reading
                // whatever happened to be in the first integer register.
                let abi = get_abi_for_conv(CallingConv::C, self.target);
                let by_address = matches!(
                    abi.classify_param(param.typ, self.types),
                    crate::abi::ArgClass::Indirect { .. }
                );
                if by_address {
                    struct_params.push((name, param.symbol, param.typ, pseudo_id));
                } else {
                    // Complex parameters: copy to local storage so real/imag access works
                    // These are passed in FP registers per ABI, so we create local
                    // storage and the codegen handles the register split.
                    complex_params.push((
                        name,
                        param.symbol,
                        param.typ,
                        pseudo_id,
                        i as u32 + arg_offset,
                    ));
                }
            } else {
                // Store all scalar parameters to locals so SSA conversion can properly
                // handle reassignment with phi nodes. If the parameter is never modified,
                // SSA will optimize away the redundant load/store.
                scalar_params.push(ScalarParam {
                    name,
                    symbol: param.symbol,
                    typ: param.typ,
                    passed_as,
                    arg: pseudo_id,
                });
            }
        }

        self.current_func = Some(ir_func);
        self.cfg_edges.clear();

        // Create entry block
        let entry_bb = self.alloc_bb();
        self.switch_bb(entry_bb);

        // Entry instruction
        self.emit(Instruction::new(Opcode::Entry));

        self.store_valist_params(valist_params);

        self.store_struct_params(struct_params);

        self.store_complex_params(complex_params);

        self.store_scalar_params(scalar_params);

        // Record the extents of any variably-modified parameter, now that
        // every parameter is stored to a local and so nameable by a later
        // one's size expression (C17 6.9.1p10 evaluates these on entry).
        //
        // `int a[n][m]` is adjusted to `int (*a)[m]`, so what needs sizing is
        // the pointee. Without this the element type has a compile-time size
        // of 0 and every row stride is 0.
        // A parameter's discarded array size is evaluated on entry for its
        // side effects and nothing else: after the array-to-pointer
        // adjustment there is no size left to record. C17 6.9.1p10 evaluates
        // it, so `int sub(int i, int array[i++])` must leave `i` at 11.
        //
        // Ahead of the extent recording below so the expressions run in the
        // order they were written, and cloned because the loop needs `self`.
        let discarded: Vec<Expr> = func
            .params
            .iter()
            .flat_map(|p| p.discarded_dims.iter().cloned())
            .collect();
        for dim in &discarded {
            self.linearize_expr(dim);
        }

        for param in &func.params {
            if param.vm_dims.is_empty() {
                continue;
            }
            let Some(symbol_id) = param.symbol else {
                continue;
            };
            let Some(pointee) = self.types.base_type(param.typ) else {
                continue;
            };

            let name = self.symbol_name(symbol_id);
            let vm_dims = param.vm_dims.clone();
            let (dims, elem_type) = self.record_vm_extents(pointee, &vm_dims, &name);

            if let Some(info) = self.locals.get_mut(&symbol_id) {
                // A parameter *is* the row: one index step off the pointer
                // consumes no extent of the pointee, so all of them remain.
                info.vm_row_dims = dims;
                info.vla_elem_type = Some(elem_type);
            }
        }

        // Linearize body
        self.linearize_stmt(&func.body);
        self.check_label_references();

        // The address-taken set is complete only now, so the dispatch block's
        // successors are linked here rather than at each computed goto.
        self.finish_indirect_dispatch();

        // Same reason: a label's depth is known only once it has been placed,
        // so a forward `goto` learns only here whether it left a VLA's scope.
        // Before SSA, which has to see the restore.
        self.resolve_forward_goto_vla_restores();

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

        // Nothing is emitted for code no path reaches, at any level: see
        // `dce::remove_unreachable_blocks`. Before SSA, which then never
        // sees a definition or a phi source in a block that cannot run.
        if let Some(ref mut ir_func) = self.current_func {
            super::dce::remove_unreachable_blocks(ir_func);
        }

        // Run SSA conversion if enabled
        if self.run_ssa {
            if let Some(ref mut ir_func) = self.current_func {
                ssa_convert(ir_func, self.types);
                // Note: ssa_convert sets ir_func.next_pseudo to account for phi nodes
                // Drop func.locals entries whose Sym is now unused, so the
                // backend regalloc allocates no stack slot for them.
                mem2reg(ir_func);
            }
        } else {
            // Only set next_pseudo if SSA was NOT run (SSA sets its own)
            if let Some(ref mut ir_func) = self.current_func {
                ir_func.next_pseudo = self.next_pseudo;
            }
        }

        // Pop function-level scope. Its VLA release is a no-op: the body's
        // own block scope dropped every mark, and the block is terminated by
        // the return above -- which is what must happen, since SSA has
        // already run over the function by this point.
        self.pop_scope(func_scope);

        // Add function to module
        if let Some(ir_func) = self.current_func.take() {
            self.module.add_function(ir_func);
        }
    }

    // Statement linearization

    /// Emit a return through the hidden pointer (sret).
    ///
    /// The value is an aggregate or a MEMORY-class complex
    /// (`returns_via_hidden_pointer`). A complex one is converted to the
    /// return type first, as the register return path does: the caller reads
    /// it with the declared base type's stride.
    pub(crate) fn emit_sret_return(&mut self, e: &Expr, sret_ptr: PseudoId, ret_type: TypeId) {
        let src_addr = if !self.types.is_complex(ret_type) {
            self.linearize_lvalue(e)
        } else if self.types.is_complex(self.expr_type(e)) {
            self.complex_operand_at_precision(e, ret_type)
        } else {
            self.promote_real_to_complex(e, ret_type)
        };
        let struct_bytes = self.types.size_bytes(ret_type);
        // The shared block copy, for the same two reasons the parameter
        // prologue uses it: a large struct becomes a `memcpy` call instead of
        // an unbounded unroll, and a size that is not a multiple of eight is
        // copied exactly rather than rounded up past the caller's object.
        // The caller's buffer is its own temporary, never a volatile object.
        let vol = BlockVolatility {
            dst: false,
            src: self.types.contains_volatile(self.expr_type(e)),
        };
        self.emit_block_copy(sret_ptr, src_addr, struct_bytes as i64, vol);

        self.emit(Instruction::ret_typed(
            Some(sret_ptr),
            self.types.void_ptr_id,
            64,
        ));
    }

    /// Whether a value of type `typ` is an aggregate the ABI returns in
    /// registers and that is wider than one: in two general registers, one
    /// SSE register holding sixteen bytes, x87, or an HFA of up to four
    /// floating members -- thirty-two bytes at most.
    ///
    /// The one answer for both ends of a call. The callee's side stopped at
    /// sixteen bytes and the caller's had no bound, so a three- or
    /// four-`double` HFA was a register aggregate to its caller and an
    /// ordinary scalar return to its callee, whose `Ret` then carried an
    /// address under no ABI classification -- which is why the inliner had to
    /// refuse every HFA and x87 return.
    pub(crate) fn returns_reg_aggregate(&self, typ: TypeId) -> bool {
        matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union)
            && self.types.size_bits(typ) > 64
            && !self.returns_via_hidden_pointer(typ)
    }

    /// Return an aggregate the ABI returns in registers
    /// ([`Self::returns_reg_aggregate`]), with the classification that says
    /// how on the `Ret`.
    pub(crate) fn emit_reg_aggregate_return(&mut self, e: &Expr, ret_type: TypeId) {
        let src_addr = self.linearize_lvalue(e);
        let struct_size = self.types.size_bits(ret_type);
        let abi = get_abi_for_conv(self.current_calling_conv, self.target);
        let ret_class = abi.classify_return(ret_type, self.types);

        // The three classes no pair of general registers can carry: an x87
        // aggregate, an HFA, and sixteen bytes in one SSE register. Each hands
        // the value back by address, and splitting any of them into RAX/RDX
        // was a miscompile of its own -- an x87 aggregate left the caller
        // reading a slot nobody had written, a `__float128` one handed a
        // gcc-compiled caller half a value in the wrong place, and an HFA's
        // two-source `Ret` spliced into a caller expecting one value dropped
        // its second half. `aggregate_ret_is_address` is where that list
        // lives, because the inliner has to ask the same question of the
        // `Ret` this emits.
        if super::aggregate_ret_is_address(&ret_class, struct_size) {
            let mut ret_insn = Instruction::ret_typed(Some(src_addr), ret_type, struct_size);
            ret_insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(vec![], ret_class)));
            self.emit(ret_insn);
            return;
        }

        // Everything else is a pair of general registers, and so at most
        // sixteen bytes: a wider aggregate that is not returned by address
        // went through the hidden pointer.
        debug_assert!(
            struct_size <= 128,
            "a two-register return of {struct_size} bits"
        );

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
        ret_insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(vec![], ret_class)));
        self.emit(ret_insn);
    }

    // Expression linearization

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
            // Writes through its third argument.
            ExprKind::CheckedArith { .. } => false,
            // Reads a hidden local the typedef already stored: no side
            // effect, and re-reading it is what makes the extent stable.
            ExprKind::VmTypedefExtent(..) | ExprKind::VmObjectExtent(..) => true,
            ExprKind::VmTypeName { dims, expr, .. } => {
                dims.iter().all(|d| self.is_pure_expr(d)) && self.is_pure_expr(expr)
            }
            // A label's address is a constant of the function.
            ExprKind::LabelAddr(_) => true,
            // The forwarding builtins read the caller's arguments and write
            // nothing. `VaArgPack` is not a value at all -- the call it sits
            // in carries it -- but it is no less pure for that.
            ExprKind::VaArgPack | ExprKind::VaArgPackLen => true,
            // Answers a question *about* its operand without evaluating it:
            // an impure one is never linearized at all (see below).
            ExprKind::ConstantP(_) => true,
            // Literals are always pure
            ExprKind::IntLit(_)
            | ExprKind::Int128Lit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit(_)
            | ExprKind::WideStringLit(_)
            | ExprKind::Utf16StringLit(_)
            | ExprKind::Utf32StringLit(_) => true,

            // Identifiers are pure unless volatile.
            //
            // `contains_volatile`, not the top-level modifier: reading a
            // struct with a `volatile` member reads that member, and asking
            // only what was written on the struct answered no.
            ExprKind::Ident(_) => match expr.typ {
                Some(typ) => !self.types.contains_volatile(typ),
                None => true,
            },

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

            // `a ?: b` evaluates `a` once and `b` only when `a` is false, so
            // it is pure exactly when both are.
            ExprKind::CondElvis { cond, else_expr } => {
                self.is_pure_expr(cond) && self.is_pure_expr(else_expr)
            }

            // Function calls are never pure (may have side effects)
            ExprKind::Call { .. } => false,

            // Member access through struct value (.) is pure if the base is
            // pure and the member itself is not volatile. C17 6.5.15p4
            // evaluates only one arm of a conditional and 5.1.2.3 makes each
            // volatile read an observable event, so speculating one is a read
            // the program never asked for: asking about the base alone let
            // `c ? s.status : s.other` load both members unconditionally into
            // a branchless select, at `-O0` too. The member's type carries the
            // object's qualifiers (C17 6.5.2.3p3), so this covers a volatile
            // member and a member of a volatile object alike.
            ExprKind::Member { expr: base, .. } => {
                !expr
                    .typ
                    .is_some_and(|typ| self.types.contains_volatile(typ))
                    && self.is_pure_expr(base)
            }

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

            // `sizeof` of a variable length array type is the one form that
            // evaluates its operand (6.5.3.4p2), and that operand can call a
            // function. Reporting it pure lets a conditional expression be
            // lowered branchlessly, which evaluates *both* arms -- so
            // `c ? sizeof(int[f()]) : 0` would call `f` even when `c` is
            // false, against 6.5.15p4.
            ExprKind::SizeofType(typ, dims) => {
                !crate::parse::ast::sizeof_type_is_runtime(self.types, *typ, dims)
                    || dims.iter().all(|d| self.is_pure_expr(d))
            }

            // `sizeof` evaluates a variably modified operand (6.5.3.4p2);
            // the other two never evaluate anything.
            ExprKind::SizeofExpr(inner) => {
                !self.sizeof_evaluates(inner) || self.is_pure_expr(inner)
            }
            ExprKind::AlignofType(_) | ExprKind::AlignofExpr(_) => true,

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
            | ExprKind::Clrsb { arg }
            | ExprKind::Clrsbl { arg }
            | ExprKind::Clrsbll { arg }
            | ExprKind::Popcount { arg }
            | ExprKind::Popcountl { arg }
            | ExprKind::Popcountll { arg }
            | ExprKind::FpTest { arg, .. } => self.is_pure_expr(arg),

            ExprKind::InlineLibraryCall {
                func, args, name, ..
            } => {
                !func.has_side_effects()
                    && !func.is_displaced(*name, &self.defined_functions)
                    && args.iter().all(|a| self.is_pure_expr(a))
            }

            // Pure iff both operands are: the relation itself reads nothing
            // else and raises nothing, which is the point of the family.
            ExprKind::FpCompare { lhs, rhs, .. } => {
                self.is_pure_expr(lhs) && self.is_pure_expr(rhs)
            }

            // Pure iff everything it reads is: the class codes are ordinary
            // expressions, not constants, so they count too.
            ExprKind::FpClassify { classes, arg } => {
                self.is_pure_expr(arg) && classes.iter().all(|c| self.is_pure_expr(c))
            }

            // Alloca allocates memory - not pure
            ExprKind::Alloca { .. } => false,

            // Unreachable is pure (no side effects, just UB hint)
            ExprKind::Unreachable => true,

            // Frame/return address builtins are pure (just read registers)
            ExprKind::FrameAddress { .. } | ExprKind::ReturnAddress { .. } => true,

            // Setjmp/longjmp have control flow side effects
            ExprKind::Setjmp { .. } | ExprKind::Longjmp { .. } => false,

            // Atomic operations have side effects (memory ordering)
            ExprKind::GnuAtomicRmw { .. }
            | ExprKind::GnuAtomicCas { .. }
            | ExprKind::C11AtomicInit { .. }
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

    /// The address of a complex-valued expression, whichever kind it is.
    ///
    /// Complex values live in memory and travel by address, and every complex
    /// path (argument, assignment, return) needs that one address. Three
    /// different expression shapes produce it three different ways:
    ///
    /// - an **lvalue** has an address to take, so `linearize_lvalue`;
    /// - a **call** returning complex yields a `Sym` pseudo whose own stack
    ///   slot *is* the returned value, so its address has to be taken;
    /// - every other **rvalue** — `__builtin_complex(...)`, `x + y` — already
    ///   lowers to a temp holding a pointer to the value, which is the answer
    ///   as it stands.
    ///
    /// Both ways of being wrong are silent and fatal: asking an rvalue for its
    /// lvalue yields a bogus pointer, and handing a consumer the value's own
    /// bits gets them dereferenced as an address. Which crash you got depended
    /// on the syntax at the use site.
    pub(crate) fn complex_operand_addr(&mut self, expr: &Expr) -> PseudoId {
        let is_lvalue = matches!(
            expr.kind,
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
            return self.linearize_lvalue(expr);
        }
        let typ = self.expr_type(expr);
        let val = self.linearize_expr(expr);
        self.rvalue_addr(val, typ)
    }

    /// The address of a value that has just been materialized.
    ///
    /// A `Sym`'s slot holds the value itself. Any other pseudo of a struct or
    /// union that fits in a register holds the aggregate's *value* -- the IR's
    /// convention, which `linearize_ident` and friends follow -- so it is
    /// stored to a temporary whose address is returned. Anything else already
    /// holds a pointer. Deciding here, where the pseudo's kind is known, is
    /// what lets every consumer treat the result as a plain address; handing
    /// one a small struct's bits instead got them dereferenced as an address,
    /// which is how `t = c ? u : v` on an eight-byte struct segfaulted.
    pub(crate) fn rvalue_addr(&mut self, val: PseudoId, typ: TypeId) -> PseudoId {
        let slot_is_the_value = self
            .current_func
            .as_ref()
            .and_then(|f| f.get_pseudo(val))
            .is_some_and(|p| matches!(p.kind, PseudoKind::Sym(_)));
        if !slot_is_the_value {
            if !self.aggregate_travels_by_value(typ) {
                return val;
            }
            let size = self.types.size_bits(typ);
            let tmp = self.frame_temp("__rvalue", typ);
            self.emit(Instruction::store(val, tmp, 0, typ, size));
            return self.rvalue_addr(tmp, typ);
        }
        let addr = self.alloc_reg_pseudo();
        let ptr_type = self.types.pointer_to(typ);
        self.emit(Instruction::sym_addr(addr, val, ptr_type));
        addr
    }

    /// Whether a struct or union of this type travels in the IR as its value
    /// rather than its address: it does when it fits in one register, the
    /// threshold `linearize_ident` applies. A complex value always travels by
    /// address, and is not asked about here.
    pub(crate) fn aggregate_travels_by_value(&self, typ: TypeId) -> bool {
        matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union)
            && (1..=64).contains(&self.types.size_bits(typ))
    }

    /// Linearize an expression as an lvalue (get its address)
    pub(crate) fn linearize_lvalue(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            // `&(int (*)[n]){ p }`: a compound literal is an lvalue, however
            // its type was written.
            ExprKind::VmTypeName { symbol, dims, expr } => {
                self.record_type_name_extents(*symbol, self.expr_type(expr), dims);
                self.linearize_lvalue(expr)
            }
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
                    // When the slot holds a pointer to the object -- a VLA's
                    // `alloca`d storage, or a `va_list` parameter -- the
                    // object's address is that pointer's value, not the
                    // slot's. Taking the slot's address made `&a` differ from
                    // `a` for every VLA, so `int (*p)[n] = &a` pointed at the
                    // pointer and read back garbage.
                    if let Storage::Indirect(ptr_type) = local.storage {
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
                    //
                    // Again only where `va_list` is an array. Where it is a
                    // pointer, the object's address is the slot's, so the
                    // parameter has to spill like any other.
                    if type_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
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
                            vla_outer_extent: None,
                            vla_elem_type: None,
                            vm_row_dims: vec![],
                            storage: Storage::InSlot,
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
                let member_info = self
                    .types
                    .find_member(struct_type, *member)
                    .unwrap_or_else(|| MemberInfo::standing_in(self.expr_type(expr)));

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
                let member_info = self
                    .types
                    .find_member(struct_type, *member)
                    .unwrap_or_else(|| MemberInfo::standing_in(self.expr_type(expr)));

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
                // Same stride rule as the rvalue path: a variably-modified
                // element type has no usable compile-time size, and this is
                // the path that `a[i][j] = v` and `&a[i][j]` take.
                let elem_size_val = self.vm_index_stride(ptr_expr).unwrap_or_else(|| {
                    let elem_size = self.types.size_bytes(elem_type);
                    self.emit_const(elem_size as i128, self.types.long_id)
                });

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
                    func.add_local(&unique_name, sym_id, *typ, self.current_bb, None);
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
            // `__real__ z` and `__imag__ z` name storage inside `z`, so they
            // are lvalues exactly when `z` is one. The imaginary half sits one
            // base type above the real one.
            ExprKind::Unary {
                op: op @ (UnaryOp::Real | UnaryOp::Imag),
                operand,
            } => {
                let op_typ = self.expr_type(operand);
                let addr = self.complex_operand_addr(operand);
                if *op == UnaryOp::Real || !self.types.is_complex(op_typ) {
                    return addr;
                }
                let base_bytes = (self.types.size_bytes(self.types.complex_base(op_typ))) as i128;
                let off = self.emit_const(base_bytes, self.types.long_id);
                let out = self.alloc_reg_pseudo();
                let ptr_type = self.types.pointer_to(self.types.complex_base(op_typ));
                self.emit(Instruction::binop(
                    Opcode::Add,
                    out,
                    addr,
                    off,
                    ptr_type,
                    self.target.pointer_width,
                ));
                out
            }
            _ => {
                // Not a designator -- a call returning a struct, a comma, a
                // conditional. Evaluating it yields the *value*; a consumer
                // that loads or stores folds the offset in and reads the right
                // bytes, but one that does arithmetic -- indexing an array
                // member, taking an address -- would dereference the value's
                // own bits. Materialize it and hand back its address instead.
                //
                // The temporary is a function-scope local, so it outlives the
                // full expression as C17 6.5.2.3p5 requires.
                let typ = self.expr_type(expr);
                let val = self.linearize_expr(expr);
                self.rvalue_addr(val, typ)
            }
        }
    }

    /// Linearize a type cast expression
    /// A cast to a complex type (C17 6.3.1.7p1).
    ///
    /// From a real: the value becomes the real part and the imaginary part is
    /// zero. From a complex: each half is converted to the new precision,
    /// which is what makes `(_Complex double)(_Complex float)z` widen both
    /// halves rather than only one.
    ///
    /// Built the same way `__builtin_complex` is -- a local of the target type
    /// and two stores -- so the result travels by address like every other
    /// complex value.
    fn emit_cast_to_complex(
        &mut self,
        inner_expr: &Expr,
        src_type: TypeId,
        cast_type: TypeId,
    ) -> PseudoId {
        let base_typ = self.types.complex_base(cast_type);
        let base_bits = self.types.size_bits(base_typ);
        let base_bytes = (base_bits / 8) as i64;

        let (real_val, imag_val) = if self.types.is_complex(src_type) {
            // Each half, converted to the destination's precision.
            let src_base = self.types.complex_base(src_type);
            let addr = self.complex_operand_addr(inner_expr);
            let src_bits = self.types.size_bits(src_base);
            let src_bytes = (src_bits / 8) as i64;

            let re = self.alloc_pseudo();
            self.emit(Instruction::load(re, addr, 0, src_base, src_bits));
            let im = self.alloc_pseudo();
            self.emit(Instruction::load(im, addr, src_bytes, src_base, src_bits));

            (
                self.emit_convert(re, src_base, base_typ),
                self.emit_convert(im, src_base, base_typ),
            )
        } else {
            // A real source: convert it, and pair it with a zero of the
            // destination's base type.
            let v = self.linearize_expr(inner_expr);
            let re = self.emit_convert(v, src_type, base_typ);
            let zero = self.emit_fconst(crate::float::FloatVal::ZERO, base_typ);
            (re, zero)
        };

        let result = self.frame_temp_addr("__ctmp", cast_type);
        self.emit(Instruction::store(real_val, result, 0, base_typ, base_bits));
        self.emit(Instruction::store(
            imag_val, result, base_bytes, base_typ, base_bits,
        ));
        result
    }

    pub(crate) fn linearize_cast(&mut self, inner_expr: &Expr, cast_type: TypeId) -> PseudoId {
        let src_type = self.expr_type(inner_expr);

        // C17 6.3.1.7p2: converting a complex value to a real type keeps the
        // real part and discards the imaginary one. Falling through to the
        // scalar path reinterpreted the address the complex value travels by
        // as the value itself, so `(double) z` produced a number in the range
        // of a pointer bit pattern.
        // `_Bool` is the exception: 6.3.1.2 converts by comparing against 0,
        // and for a complex value that comparison is against `0.0 + 0.0i`, so
        // the imaginary half decides the answer too. Keeping only the real
        // part here made `(_Bool)(0.0 + 3.0i)` false.
        if self.types.is_complex(src_type) && self.types.kind(cast_type) == TypeKind::Bool {
            return self.emit_complex_nonzero(inner_expr);
        }

        // C17 6.3.2.2: a cast to `void` evaluates the operand and discards
        // the value, converting nothing. This has to come before the complex
        // arms as well as before the arithmetic ones: `void` is neither
        // complex nor floating, so a complex operand took the
        // complex-to-real arm just below and a floating one the
        // float-to-integer arm further down, and `(void)z` became a
        // `cvttsd2si` that raises `FE_INVALID` for a NaN. The optimizer
        // deleted the dead conversion at -O1 and above, so only -O0 raised
        // it.
        if self.types.kind(cast_type) == TypeKind::Void {
            return self.linearize_expr(inner_expr);
        }

        if self.types.is_complex(src_type) && !self.types.is_complex(cast_type) {
            return self.emit_complex_to_real(inner_expr, cast_type);
        }

        // C17 6.3.1.7p1: converting a real to a complex type gives the real
        // value as the real part and a zero imaginary part; converting complex
        // to complex converts each part. Neither had a branch here, so a cast
        // *to* a complex type fell through to the scalar path and returned a
        // value where a complex address was expected -- `(_Complex double)0.0`
        // segfaulted at every precision, from a real or from a complex source.
        if self.types.is_complex(cast_type) {
            return self.emit_cast_to_complex(inner_expr, src_type, cast_type);
        }

        let src = self.linearize_expr(inner_expr);

        // C17 6.3.1.2: a conversion to `_Bool` compares against zero. It is
        // not a truncation, and `_Bool` is an integer type, so a floating
        // operand fell into the float-to-integer arm below and `(_Bool)0.5`
        // became a `cvttsd2si` -- 0, where the value is plainly not zero.
        // `emit_convert` carries the rule for every source type; this path
        // has a conversion of its own and reached it only for an integer.
        if self.types.kind(cast_type) == TypeKind::Bool
            && self.types.kind(src_type) != TypeKind::Bool
        {
            return self.emit_convert(src, src_type, cast_type);
        }

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
            // Float to float conversion (e.g., float to double).
            //
            // Two floating types need a conversion unless they are the same
            // format; equal *width* does not mean equal format. x87 extended
            // and IEEE binary128 are both 128 bits wide here, and comparing
            // widths elided every `long double` <-> `__float128` cast, leaving
            // one format's bytes to be read as the other's.
            let src_size = self.types.size_bits(src_type);
            let dst_size = self.types.size_bits(cast_type);
            if self.types.kind(src_type) != self.types.kind(cast_type) {
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
                src // Same format, no conversion needed
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
    ///
    /// `access_typ` is the type of the member-access *expression*, which the
    /// parser formed as the member's declared type so-qualified by the object
    /// (C17 6.5.2.3p3/p4). The access is performed at that type, so
    /// [`Self::mark_volatile_access`] sees the qualifier -- the type
    /// `find_member` answers with is the member's *declared* one and cannot
    /// carry it, which is why a member of a `volatile` struct read as an
    /// ordinary `int` and DCE deleted the load from `-O1` up. Its width, sign
    /// and kind still come from the member, so the two disagreeing (only
    /// reachable once the parser has already reported an unknown member)
    /// cannot change how the access is performed. It also stands in for the
    /// member type entirely when the lookup fails here.
    pub(crate) fn emit_member_access(
        &mut self,
        base: PseudoId,
        struct_type: TypeId,
        member: StringId,
        access_typ: TypeId,
    ) -> PseudoId {
        let member_info = self
            .types
            .find_member(struct_type, member)
            .unwrap_or_else(|| MemberInfo::standing_in(access_typ));

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
            member_info.access_bytes,
        ) {
            // Bitfield read
            self.emit_bitfield_load(
                base,
                member_info.offset,
                bit_offset,
                bit_width,
                storage_size,
                access_typ,
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
                    access_typ,
                    size,
                ));
                result
            }
        }
    }

    /// Bytes that one step of `ptr_expr` spans, as a run-time value.
    ///
    /// A variably-modified pointee reports a compile-time size of 0, so the
    /// stride has to come from the object's recorded extents. Every place
    /// that scales by a pointee's size goes through here -- `p[i]`, `p + i`,
    /// `p - q`, `++p`, `p++`, `p += i` -- rather than each working it out
    /// separately.
    pub(crate) fn pointer_step_bytes(&mut self, ptr_expr: &Expr, ptr_typ: TypeId) -> PseudoId {
        if let Some(stride) = self.vm_index_stride(ptr_expr) {
            return stride;
        }
        let elem_type = self.types.base_type(ptr_typ).unwrap_or(self.types.char_id);
        let elem_size = self.types.size_bytes(elem_type);
        self.emit_const(elem_size as i128, self.types.long_id)
    }

    /// Record the extents of a variably-modified `array_type`, evaluating each
    /// run-time one into a hidden local so it survives SSA.
    ///
    /// Returns the extents outermost-first, one per array level, together with
    /// the innermost non-array element type. `vm_exprs` supplies a size
    /// expression for each level whose extent is not a constant, in order, so
    /// `int a[n][4][m]` consumes `n` then `m`.
    ///
    /// Both a local VLA declaration and a variably-modified parameter go
    /// through here; keeping one implementation is the point, since the
    /// original defect was a second path that never recorded extents at all.
    pub(crate) fn record_vm_extents(
        &mut self,
        array_type: TypeId,
        vm_exprs: &[Expr],
        name: &str,
    ) -> (Vec<VmDim>, TypeId) {
        let ulong_type = self.types.ulong_id;
        let mut dims: Vec<VmDim> = Vec::new();
        let mut exprs = vm_exprs.iter();
        let mut elem_type = array_type;
        // An unsized level with no expression is incomplete -- the `[]` of
        // `int (*p)[][m]` -- and only the outermost may be (6.7.6.2p1), so
        // the expressions belong to the innermost unsized levels. Handing
        // them out from the outside gave `m` to the `[]` and left the row
        // extent 0.
        let mut incomplete = self
            .types
            .unsized_array_levels(array_type)
            .saturating_sub(vm_exprs.len());

        while self.types.kind(elem_type) == TypeKind::Array {
            let level = elem_type;
            elem_type = self.types.base_type(level).unwrap_or(self.types.int_id);

            if let Some(n) = self.types.get(level).array_size {
                dims.push(VmDim::Const(n));
                continue;
            }

            let next = if incomplete == 0 { exprs.next() } else { None };
            let Some(size_expr) = next else {
                // Nothing to evaluate and nothing measurable; the entry
                // keeps every later level at its own index.
                incomplete = incomplete.saturating_sub(1);
                dims.push(VmDim::Const(0));
                continue;
            };

            let dim_size = self.linearize_expr(size_expr);

            let dim_sym_id = self.alloc_pseudo();
            let dim_var_name = format!("__vla_dim{}_{}.{}", dims.len(), name, dim_sym_id.0);
            let dim_sym = Pseudo::sym(dim_sym_id, dim_var_name.clone());
            if let Some(func) = &mut self.current_func {
                func.add_pseudo(dim_sym);
                func.add_local(
                    &dim_var_name,
                    dim_sym_id,
                    ulong_type,
                    self.current_bb,
                    None, // no explicit alignment
                );
            }

            // Widen to 64-bit before storing.
            let dim_expr_typ = size_expr.typ.unwrap_or(self.types.int_id);
            let dim_size = self.emit_convert(dim_size, dim_expr_typ, ulong_type);
            self.emit(Instruction::store(dim_size, dim_sym_id, 0, ulong_type, 64));
            dims.push(VmDim::Sym(dim_sym_id));
        }

        (dims, elem_type)
    }

    /// The product of `dims`, evaluated at run time.
    ///
    /// None for no extents at all, which is an empty product: the caller
    /// decides whether that means one element or no computation to do.
    pub(crate) fn vm_extent_product(&mut self, dims: &[VmDim]) -> Option<PseudoId> {
        let ulong = self.types.ulong_id;
        let mut acc: Option<PseudoId> = None;
        for dim in dims {
            let val = match *dim {
                VmDim::Const(n) => self.emit_const(n as i128, ulong),
                VmDim::Sym(sym) => {
                    // Reload at the point of use: the extent lives in a hidden
                    // local precisely so it survives across basic blocks.
                    let loaded = self.alloc_pseudo();
                    self.emit(Instruction::load(loaded, sym, 0, ulong, 64));
                    loaded
                }
            };
            acc = Some(match acc {
                None => val,
                Some(prev) => {
                    let result = self.alloc_pseudo();
                    self.emit(Instruction::binop(
                        Opcode::Mul,
                        result,
                        prev,
                        val,
                        ulong,
                        64,
                    ));
                    result
                }
            });
        }
        acc
    }

    /// Byte size of `product(dims) * sizeof(elem)`, evaluated at run time, or
    /// None when the compile-time size is already correct.
    ///
    /// None means either that no extents remain -- the object is the
    /// fully-indexed element -- or that every remaining extent is constant, in
    /// which case the type carries its own size and no arithmetic is needed.
    fn vm_extent_size(&mut self, dims: &[VmDim], elem: TypeId) -> Option<PseudoId> {
        if !dims.iter().any(|d| matches!(d, VmDim::Sym(_))) {
            return None;
        }

        let ulong = self.types.ulong_id;
        let acc = self.vm_extent_product(dims);

        let elem_size = self.types.size_bytes(elem) as i128;
        let elem_size_val = self.emit_const(elem_size, ulong);
        let total = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Mul,
            total,
            acc.expect("a Sym extent guarantees an accumulator"),
            elem_size_val,
            ulong,
            64,
        ));
        Some(total)
    }

    /// Lower a `__builtin_isnan` / `isinf` / `isfinite` / `isnormal` /
    /// `signbit`.
    ///
    /// Built from comparisons alone, which keeps them exact at every width and
    /// needs no new opcode or backend work -- except `signbit`, which asks
    /// what no comparison can see, the sign of a zero or a NaN.
    ///
    ///   isnan(x)     x != x                  (only a NaN differs from itself)
    ///   isinf(x)     x == +inf || x == -inf
    ///   isfinite(x)  x > -inf && x < +inf    (a NaN fails both: unordered)
    ///   isnormal(x)  isfinite(x) && (x >= +min_normal || x <= -min_normal)
    ///
    /// The argument is linearized once into a pseudo and that pseudo is reused.
    /// Desugaring to an AST node instead would duplicate the argument
    /// expression, so `isnan(f())` would call `f` twice.
    fn linearize_fp_test(&mut self, test: FpTest, arg: &Expr) -> PseudoId {
        let typ = self.expr_type(arg);
        let val = self.linearize_expr(arg);

        match test {
            FpTest::IsNan => self.emit_compare(Opcode::FCmpONe, val, val, typ),
            FpTest::IsInf => {
                let pos = self.emit_fconst(FloatVal::infinity(false), typ);
                let neg = self.emit_fconst(FloatVal::infinity(true), typ);
                let a = self.emit_compare(Opcode::FCmpOEq, val, pos, typ);
                let b = self.emit_compare(Opcode::FCmpOEq, val, neg, typ);
                self.emit_bool_combine(Opcode::Or, a, b)
            }
            FpTest::IsInfSign => {
                // +1 for +inf, -1 for -inf, 0 otherwise: the two equality
                // tests are each 0 or 1, so their difference carries the sign.
                let pos = self.emit_fconst(FloatVal::infinity(false), typ);
                let neg = self.emit_fconst(FloatVal::infinity(true), typ);
                let a = self.emit_compare(Opcode::FCmpOEq, val, pos, typ);
                let b = self.emit_compare(Opcode::FCmpOEq, val, neg, typ);
                self.emit_bool_combine(Opcode::Sub, a, b)
            }
            FpTest::IsFinite => self.emit_is_finite(val, typ),
            FpTest::IsNormal => {
                let finite = self.emit_is_finite(val, typ);
                let magnitude = self.emit_at_least_normal(val, typ);
                self.emit_bool_combine(Opcode::And, finite, magnitude)
            }
            FpTest::SignBit => self.emit_signbit(val, typ),
        }
    }

    /// The C99 7.12.14 relations, each yielding 0 or 1.
    ///
    /// Every one of them is a comparison c17 already emits. The family exists
    /// in C because the ordinary relational operators are specified to raise
    /// `FE_INVALID` on an unordered pair and these are not -- and c17 emits
    /// the quiet compare (`ucomis*`, `fucomip`) for both, so the two agree
    /// here and there is nothing further to arrange.
    ///
    /// Both operands are linearized before any comparison is emitted, so
    /// `isgreater(f(), g())` calls each function exactly once.
    fn linearize_fp_compare(&mut self, cmp: FpCompare, lhs: &Expr, rhs: &Expr) -> PseudoId {
        let typ = self.expr_type(lhs);
        let a = self.linearize_expr(lhs);
        let b = self.linearize_expr(rhs);

        if let Some(op) = super::linearize_emit::fp_compare_opcode(cmp) {
            return self.emit_compare(op, a, b, typ);
        }
        match cmp {
            // Ordered and unequal. `!=` will not do: it is *true* for an
            // unordered pair, and this must be false for one.
            FpCompare::LessGreater => {
                let below = self.emit_compare(Opcode::FCmpOLt, a, b, typ);
                let above = self.emit_compare(Opcode::FCmpOGt, a, b, typ);
                self.emit_bool_combine(Opcode::Or, below, above)
            }
            // `isnan(a) || isnan(b)`, spelled as the self-comparison
            // `linearize_fp_test` uses for `isnan`.
            FpCompare::Unordered => {
                let a_nan = self.emit_compare(Opcode::FCmpONe, a, a, typ);
                let b_nan = self.emit_compare(Opcode::FCmpONe, b, b, typ);
                self.emit_bool_combine(Opcode::Or, a_nan, b_nan)
            }
            _ => unreachable!("{cmp:?} is one comparison"),
        }
    }

    /// `x > -inf && x < +inf`, which is false for a NaN because both
    /// comparisons against one are false.
    fn emit_is_finite(&mut self, val: PseudoId, typ: TypeId) -> PseudoId {
        let pos = self.emit_fconst(FloatVal::infinity(false), typ);
        let neg = self.emit_fconst(FloatVal::infinity(true), typ);
        let below = self.emit_compare(Opcode::FCmpOLt, val, pos, typ);
        let above = self.emit_compare(Opcode::FCmpOGt, val, neg, typ);
        self.emit_bool_combine(Opcode::And, below, above)
    }

    /// `x >= +min_normal || x <= -min_normal` -- true for anything at least as
    /// large as the smallest normal, in either direction. Combined with a
    /// finiteness test this is exactly `isnormal`.
    fn emit_at_least_normal(&mut self, val: PseudoId, typ: TypeId) -> PseudoId {
        let smallest = self.smallest_normal(typ);
        let pos = self.emit_fconst(smallest, typ);
        let neg = self.emit_fconst(smallest.negated(), typ);
        let a = self.emit_compare(Opcode::FCmpOGe, val, pos, typ);
        let b = self.emit_compare(Opcode::FCmpOLe, val, neg, typ);
        self.emit_bool_combine(Opcode::Or, a, b)
    }

    /// Combine two comparison results. Both are already 0 or 1, so the
    /// bitwise operation is the logical one and no branch is needed.
    fn emit_bool_combine(&mut self, op: Opcode, lhs: PseudoId, rhs: PseudoId) -> PseudoId {
        let int_typ = self.types.int_id;
        let result = self.alloc_pseudo();
        self.emit(Instruction::binop(op, result, lhs, rhs, int_typ, 32));
        result
    }

    /// The smallest positive normal value of a floating type: 2^min_exp.
    ///
    /// Exact at every width, including x87's 2^-16382, which an `f64` cannot
    /// represent at all -- carrying literals at target precision is what makes
    /// `isnormal` on a `long double` expressible.
    fn smallest_normal(&self, typ: TypeId) -> FloatVal {
        let exp = match self.types.size_bits(typ) {
            0..=16 => -14,   // binary16
            17..=32 => -126, // binary32
            33..=64 => -1022,
            // x87 80-bit and IEEE binary128 share a minimum exponent.
            _ => -16382,
        };
        FloatVal::from_parts(false, 1, exp)
    }

    /// Lower `__builtin_fpclassify(nan, inf, normal, subnormal, zero, x)`.
    ///
    /// A chain of selects over the same tests, applied most-specific last so
    /// the earlier answers win. Subnormal is the innermost default: not a
    /// NaN, not infinite, not zero and below the smallest normal leaves
    /// nothing else it could be.
    fn linearize_fp_classify(&mut self, classes: &[Expr], arg: &Expr) -> PseudoId {
        let typ = self.expr_type(arg);
        let val = self.linearize_expr(arg);
        let int_typ = self.types.int_id;

        let nan_code = self.linearize_expr(&classes[0]);
        let inf_code = self.linearize_expr(&classes[1]);
        let normal_code = self.linearize_expr(&classes[2]);
        let subnormal_code = self.linearize_expr(&classes[3]);
        let zero_code = self.linearize_expr(&classes[4]);

        let is_nan = self.emit_compare(Opcode::FCmpONe, val, val, typ);

        let pos_inf = self.emit_fconst(FloatVal::infinity(false), typ);
        let neg_inf = self.emit_fconst(FloatVal::infinity(true), typ);
        let eq_pos = self.emit_compare(Opcode::FCmpOEq, val, pos_inf, typ);
        let eq_neg = self.emit_compare(Opcode::FCmpOEq, val, neg_inf, typ);
        let is_inf = self.emit_bool_combine(Opcode::Or, eq_pos, eq_neg);

        let zero = self.emit_fconst(FloatVal::ZERO, typ);
        let is_zero = self.emit_compare(Opcode::FCmpOEq, val, zero, typ);

        let finite = self.emit_is_finite(val, typ);
        let magnitude = self.emit_at_least_normal(val, typ);
        let is_normal = self.emit_bool_combine(Opcode::And, finite, magnitude);

        let mut acc = subnormal_code;
        for (cond, code) in [
            (is_normal, normal_code),
            (is_zero, zero_code),
            (is_inf, inf_code),
            (is_nan, nan_code),
        ] {
            let next = self.alloc_pseudo();
            self.emit(Instruction::select(next, cond, code, acc, int_typ, 32));
            acc = next;
        }
        acc
    }

    /// Stride for one index step into `ptr_expr`, when what it denotes is a
    /// variably-modified array reached by indexing a local.
    ///
    /// One step spans the object one step further in, whether `ptr_expr` is
    /// a pointer or an array that decays to one.
    fn vm_index_stride(&mut self, ptr_expr: &Expr) -> Option<PseudoId> {
        let (symbol_id, depth) = ptr_expr.vm_index_base()?;
        let info = self.locals.get(&symbol_id)?;
        let elem = info.vla_elem_type?;
        let dims = Self::vm_dims_at(info, depth + 1)?;
        self.vm_extent_size(&dims, elem)
    }

    /// Run-time size of the object `expr` denotes, when it is a
    /// variably-modified array reached by indexing a local.
    ///
    /// `sizeof(a[0])` on `int a[n][m]` is `m * sizeof(int)`; the type alone
    /// reports 0.
    fn vm_sizeof_expr(&mut self, expr: &Expr) -> Option<PseudoId> {
        if self.types.kind(self.expr_type(expr)) != TypeKind::Array {
            return None;
        }
        let (dims, elem) = self.vm_type_extents(expr)?;
        self.vm_extent_size(&dims, elem)
    }

    /// The extents of `expr`'s variably modified type, outermost first and
    /// one per array level, with the innermost element type: an array's own
    /// levels, or a pointer's pointee's -- the levels a declarator's size
    /// expressions describe.
    ///
    /// None unless `expr` is rooted in a local -- or a type-name's value --
    /// whose declaration recorded extents. A pointer's are those of what it
    /// points at, one step further in than the pointer itself.
    fn vm_type_extents(&self, expr: &Expr) -> Option<(Vec<VmDim>, TypeId)> {
        let (symbol_id, depth) = expr.vm_index_base()?;
        let info = self.locals.get(&symbol_id)?;
        let elem = info.vla_elem_type?;
        let is_pointer = self.types.kind(self.expr_type(expr)) != TypeKind::Array;
        let dims = Self::vm_dims_at(info, depth + isize::from(is_pointer))?;
        Some((dims, elem))
    }

    /// The extents of the object `level` index steps into the local `info`
    /// describes. `vm_row_dims` is what one step leaves, so step `l` has
    /// `vm_row_dims[l - 1..]`; step 0 is the local itself, which for a VLA
    /// adds back the outermost extent no step sees.
    fn vm_dims_at(info: &LocalVarInfo, level: isize) -> Option<Vec<VmDim>> {
        match usize::try_from(level).ok()? {
            0 => Some(
                std::iter::once(info.vla_outer_extent?)
                    .chain(info.vm_row_dims.iter().copied())
                    .collect(),
            ),
            l => info.vm_row_dims.get(l - 1..).map(<[VmDim]>::to_vec),
        }
    }

    /// Whether `sizeof` evaluates `inner`: C17 6.5.3.4p2 evaluates an
    /// operand of variable length array type. A bare identifier is left out,
    /// since evaluating one has no effect.
    ///
    /// Asked of the declarations, not of the extents recorded: an operand
    /// rooted in a type-name's value records its extents only as it is
    /// evaluated, so `sizeof *(int (*)[n])p` has none until it has been.
    fn sizeof_evaluates(&self, inner: &Expr) -> bool {
        !matches!(inner.kind, ExprKind::Ident(_))
            && self.types.kind(self.expr_type(inner)) == TypeKind::Array
            && crate::parse::ast::vm_extent_count(self.types, self.symbols, inner) > 0
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

        // A variably-modified element type reports a compile-time size of 0,
        // so the stride has to come from the object's recorded extents. This
        // covers every depth -- `b[i]`, `b[i][j]`, ... -- and locals and
        // parameters alike, because both record their element type's extents.
        let elem_size_val = self.vm_index_stride(ptr_expr).unwrap_or_else(|| {
            let elem_size = self.types.size_bytes(elem_type);
            self.emit_const(elem_size as i128, self.types.long_id)
        });

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

    /// Storage for a call returning `typ` through the hidden pointer, and
    /// the pointer to it, which is passed as the call's first argument.
    ///
    /// The call itself hands the same pointer back (in RAX on x86-64), so its
    /// target is a pointer-typed value; the result the caller reads is
    /// [`HiddenReturnSlot::storage`].
    pub(crate) fn hidden_return_slot(&mut self, typ: TypeId) -> HiddenReturnSlot {
        let storage = self.frame_temp("__sret", typ);
        let arg_typ = self.types.pointer_to(typ);
        let arg = self.alloc_reg_pseudo();
        self.emit(Instruction::sym_addr(arg, storage, arg_typ));
        HiddenReturnSlot {
            storage,
            arg,
            arg_typ,
        }
    }

    /// Linearize a function call expression
    pub(crate) fn linearize_call(
        &mut self,
        expr: &Expr,
        func_expr: &Expr,
        args: &[Expr],
        binding: crate::parse::ast::CalleeBinding,
        known: Option<crate::parse::ast::LibFn>,
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
        let returns_large_struct = self.returns_via_hidden_pointer(typ);
        // An aggregate that comes back in registers still needs somewhere to
        // land, and the backend writes the registers into this local. There is
        // no upper size bound: an HFA of four `double`s is thirty-two bytes
        // and still returns in registers, not through the hidden pointer.
        let returns_reg_aggregate = self.returns_reg_aggregate(typ);
        // A complex value the ABI returns in registers (x87 or SSE) is handed
        // back as the address of its halves; one it returns in memory --
        // `_Float128 _Complex` on x86-64 -- came back through the hidden
        // pointer above.
        let ret_is_address = self.types.is_complex(typ) && !returns_large_struct;

        let (result_sym, mut arg_vals, mut arg_types_vec) = if returns_large_struct {
            let slot = self.hidden_return_slot(typ);
            (slot.storage, vec![slot.arg], vec![slot.arg_typ])
        } else if returns_reg_aggregate {
            // Two-register struct returns: allocate local storage for the result
            // Codegen will store RAX+RDX (x86-64) or X0+X1 (AArch64) to this location
            let local_sym = self.frame_temp("__2reg", typ);
            (local_sym, Vec::new(), Vec::new())
        } else if ret_is_address {
            // Complex returns: allocate local storage for the result
            // Complex values are 16 bytes and need stack storage
            let local_sym = self.frame_temp("__cret", typ);
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
            let local_sym = self.frame_temp("__sret1", typ);
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
        // C17 6.5.2.2p1 lets the function designator be a function *or* a
        // pointer to one, and the prototype lives on the function type either
        // way. Reading `params` off the pointer found nothing, so a call
        // through a pointer converted no argument at all:
        //
        //   void f(double); void (*p)(double) = f; p(1);
        //
        // passed the integer 1 where a `double` was expected and the callee
        // read 0. With a mixed list every later argument moved as well.
        let formal_param_types: Option<Vec<TypeId>> = func_expr.typ.and_then(|ft_id| {
            let resolved = if self.types.kind(ft_id) == TypeKind::Pointer {
                self.types.base_type(ft_id).unwrap_or(ft_id)
            } else {
                ft_id
            };
            self.types.get(resolved).params.clone()
        });
        // A call through a function type with no prototype: C17 6.5.2.2p6
        // gives every argument the default argument promotions, as it does
        // a variadic one, and an identifier-list definition receives them so
        // (see `ParamStyle`). Passing a `float` as a float had a gcc-compiled
        // K&R callee read a double out of a register that held a single, and
        // a `char` took Apple arm64's one-byte stack slot where the callee
        // reads an `int`.
        let unprototyped = func_expr.typ.is_some_and(|ft_id| {
            let resolved = if self.types.kind(ft_id) == TypeKind::Pointer {
                self.types.base_type(ft_id).unwrap_or(ft_id)
            } else {
                ft_id
            };
            self.types.kind(resolved) == TypeKind::Function
                && self.types.get(resolved).params.is_none()
        });

        // Linearize regular arguments
        // For large structs, pass by reference (address) instead of by value
        // Note: We pass structs > 64 bits by reference. While the ABI allows
        // two-register passing for 9-16 byte structs, we don't implement that yet.
        // For complex types, pass address so codegen can load real/imag into XMM registers
        // For arrays (including VLAs), decay to pointer
        // `__builtin_va_arg_pack()` is not an argument -- it stands for the
        // caller's whole argument list, which is not known until the enclosing
        // function is inlined. Lift it off here and record it on the call; the
        // inliner appends the real arguments. 6.10 has nothing to say about
        // this; the rule is GCC's, and GCC likewise allows it only last.
        let mut ends_with_va_arg_pack = false;
        let args: &[Expr] = match args.split_last() {
            Some((last, rest)) if matches!(last.kind, ExprKind::VaArgPack) => {
                ends_with_va_arg_pack = true;
                rest
            }
            _ => args,
        };

        for (arg_idx, a) in args.iter().enumerate() {
            let mut arg_type = self.expr_type(a);
            let arg_kind = self.types.kind(arg_type);
            // Computed before the dispatch below so the complex-argument arm
            // can be skipped for a `_Bool` parameter without re-borrowing.
            let bool_param_for_complex_arg = formal_param_types
                .as_ref()
                .and_then(|params| params.get(arg_idx).copied())
                .filter(|pt| {
                    self.types.is_complex(arg_type) && self.types.kind(*pt) == TypeKind::Bool
                });
            let arg_val = if (arg_kind == TypeKind::Struct || arg_kind == TypeKind::Union)
                && self.types.size_bits(arg_type) > 64
            {
                let size_bits = self.types.size_bits(arg_type);
                let abi = get_abi_for_conv(self.current_calling_conv, self.target);
                let class = abi.classify_param(arg_type, self.types);
                if size_bits > 128 {
                    // Large struct (> 16 bytes): keep struct type so ABI classifies as
                    // Indirect/MEMORY. The pseudo is the struct's address: System V
                    // copies its bytes to the stack there, and AAPCS64 is handed a
                    // copy of it below.
                    arg_types_vec.push(arg_type);
                } else {
                    // Medium struct (9-16 bytes): the ABI classification decides
                    let is_two_fp_regs = matches!(
                        class,
                        crate::abi::ArgClass::Direct { ref classes, .. }
                            if !classes.is_empty()
                                && classes.iter().all(|c| *c == crate::abi::RegClass::Sse)
                    ) || matches!(class, crate::abi::ArgClass::Hfa { .. });
                    // MEMORY class means the bytes go on the stack by value,
                    // exactly as an over-sixteen-byte struct already does.
                    // Reachable at this size only when an eightbyte holds a
                    // `long double`, directly or merged with something else;
                    // passing a pointer instead disagreed with gcc silently.
                    let is_memory = matches!(class, crate::abi::ArgClass::Indirect { .. });
                    // Any other two-eightbyte `Direct` class -- two integer
                    // registers, or one of each -- travels in registers too,
                    // on both targets. The pseudo still carries the address;
                    // it is the backend that loads the pair out of it.
                    let is_reg_pair = matches!(
                        class,
                        crate::abi::ArgClass::Direct { ref classes, .. }
                            if classes.len() == 2
                    );
                    if is_two_fp_regs || is_memory || is_reg_pair {
                        // Keep the struct type: the ABI decides from it, and
                        // the pseudo carries the address either way.
                        arg_types_vec.push(arg_type);
                    } else {
                        // Integer or mixed struct: pass as pointer (existing behavior)
                        arg_types_vec.push(self.types.pointer_to(arg_type));
                    }
                }
                // A struct argument travels by address. `linearize_lvalue`
                // materializes an rvalue -- a call returning a struct -- and
                // hands back the temporary's address, so both cases are the
                // same call.
                let addr = self.linearize_lvalue(a);
                if matches!(class, crate::abi::ArgClass::Indirect { .. })
                    && abi.indirect_param_is_reference()
                {
                    // AAPCS64 B.4: the callee owns the memory it is pointed at
                    // and may write it, so it must be a copy. Passing the
                    // original's address was invisible c17-to-c17 -- a c17
                    // callee copies out of it first -- but a gcc callee that
                    // assigned to its parameter wrote through into the
                    // caller's object.
                    let copy = self.frame_temp("__argcopy", arg_type);
                    let copy_addr = self.alloc_reg_pseudo();
                    self.emit(Instruction::sym_addr(
                        copy_addr,
                        copy,
                        self.types.pointer_to(arg_type),
                    ));
                    let bytes = self.types.size_bytes(arg_type) as i64;
                    let vol = self.block_volatility(arg_type, self.expr_type(a));
                    self.emit_block_copy(copy_addr, addr, bytes, vol);
                    copy_addr
                } else {
                    addr
                }
            } else if bool_param_for_complex_arg.is_some() {
                // A complex argument bound to a `_Bool` parameter converts by
                // comparing against zero, so it must not take the
                // pass-the-address arm below -- see `complex_to_bool`.
                let pt = bool_param_for_complex_arg.unwrap();
                arg_types_vec.push(pt);
                self.emit_complex_nonzero(a)
            } else if let Some(pt) = formal_param_types
                .as_ref()
                .and_then(|params| params.get(arg_idx).copied())
                .filter(|pt| self.types.is_complex(*pt) && !self.types.is_complex(arg_type))
            {
                // A *real* argument bound to a complex parameter. C17
                // 6.5.2.2p2 converts it as if by assignment, and 6.3.1.7p1
                // gives the imaginary half a zero -- so the callee is handed a
                // complex object, by address, like any other complex argument.
                //
                // The arm below keys on the *argument's* type, so this case
                // reached the ordinary scalar path and the raw value was
                // passed where an address was expected: `f(7)` with a
                // `_Complex double` parameter arrived as garbage, and with a
                // `_Complex int` one the callee dereferenced the number 7.
                arg_types_vec.push(pt);
                self.promote_real_to_complex(a, pt)
            } else if self.types.is_complex(arg_type) {
                // Complex types: pass address, codegen loads real/imag into XMM registers
                // Type stays as complex (not pointer) so codegen knows it's complex.
                //
                // Converted to the parameter's own precision first. A complex
                // value is read with its base type's stride, so handing a
                // `float _Complex` to a `double _Complex` parameter without
                // converting had the callee read an 8-byte-strided pair out of
                // 4-byte-strided storage: `1.0f + 2.0f*I` arrived as `2+1i`.
                // The type recorded for the ABI has to move with it, or the
                // classification is made for a width that is no longer there.
                let param_typ = formal_param_types
                    .as_ref()
                    .and_then(|params| params.get(arg_idx).copied())
                    .filter(|pt| self.types.is_complex(*pt));
                match param_typ {
                    Some(pt) => {
                        arg_types_vec.push(pt);
                        self.complex_operand_at_precision(a, pt)
                    }
                    // No prototype, or a variadic argument: nothing says what
                    // precision the callee wants, so it travels as written.
                    None => {
                        arg_types_vec.push(arg_type);
                        self.complex_operand_addr(a)
                    }
                }
            } else if arg_kind == TypeKind::Array {
                // Array decay to pointer (C99 6.3.2.1)
                // This applies to both fixed-size arrays and VLAs
                let elem_type = self.types.base_type(arg_type).unwrap_or(self.types.int_id);
                arg_types_vec.push(self.types.pointer_to(elem_type));
                self.linearize_expr(a)
            } else if arg_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
                // va_list decay to pointer (C99 7.15.1)
                // va_list is defined as __va_list_tag[1] (an array), so it decays to
                // a pointer when passed to a function taking va_list parameter.
                // Where va_list is already a pointer there is nothing to decay, and
                // the ordinary scalar path below passes it by value.
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
                            // Integer widening (int→long, long→__int128, ...),
                            // decided by the two sizes alone: nothing about it
                            // stops at 64 bits.
                            (arg_is_int && param_is_int && arg_size < param_size)
                            // Integer narrowing (long→int, int→char, ...).
                            // The callee reads only the parameter's own bytes,
                            // but the ABI places the argument by the
                            // *parameter's* type: Apple arm64 stacks a `char`
                            // in one byte, so `f(..., 'a')` recorded as `int`
                            // took four and moved every later argument.
                            || (arg_is_int && param_is_int && arg_size > param_size)
                            // FP size mismatch (float→double, long double→double, etc.)
                            || (arg_is_fp && param_is_fp && arg_size != param_size)
                            // Integer to FP (uint32_t→double, int→float, etc.)
                            || (arg_is_int && param_is_fp)
                            // FP to integer (rare but legal)
                            || (arg_is_fp && param_is_int)
                            // To `_Bool`, whose conversion is `!= 0` and not
                            // a truncation (C17 6.3.1.2): `f(42)` with a
                            // `_Bool` parameter must pass 1.
                            || self.types.kind(param_type) == TypeKind::Bool;

                        if needs_convert {
                            val = self.emit_convert(val, arg_type, param_type);
                            arg_type = param_type;
                        }
                    }
                }

                // C99 6.5.2.2p7: default argument promotions for variadic args,
                // and 6.5.2.2p6 for every argument of an unprototyped call.
                //
                // Both halves have to happen here. The formal-parameter
                // conversion above is guarded by `arg_idx < params.len()`, and
                // a variadic argument is by definition at or past that bound,
                // so it never runs for these. The cast itself emits no IR
                // either, because emit_convert short-circuits same-size integer
                // conversions -- without an explicit promotion the pseudo still
                // holds the sign-extended load, and `printf("%02x", (unsigned
                // char)c)` prints ffffff80 for a negative `signed char`.
                if unprototyped || variadic_arg_start.is_some_and(|v| arg_idx >= v) {
                    let promoted = self.types.default_argument_promote(arg_type);
                    if promoted != arg_type {
                        val = self.emit_convert(val, arg_type, promoted);
                        arg_type = promoted;
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
        // A `transparent_union` argument travels as its first member, so that
        // is the type the backends have to be told. They do not consult the
        // ABI classification alone: aarch64's `is_fp` asks `types.is_float`,
        // and a union is not a float however it is classified, so the value
        // went out in a general-purpose register while the callee read it
        // from a V register. Substituted here, once, after the argument
        // values have been materialized and after the front end has checked
        // the call against the union's members.
        for t in arg_types_vec.iter_mut() {
            if let Some(first) = self.types.transparent_union_first_member(*t) {
                *t = first;
            }
        }

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
            call_insn.extra_mut().variadic_arg_start = variadic_arg_start;
            call_insn.extra_mut().ends_with_va_arg_pack = ends_with_va_arg_pack;
            call_insn.extra_mut().is_noreturn_call = is_noreturn_call;
            call_insn.extra_mut().callee_binding = binding;
            call_insn.extra_mut().known = known;
            call_insn.extra_mut().abi_info = Some(call_abi_info);
            self.emit(call_insn);
            if is_noreturn_call {
                self.emit_no_return(
                    Instruction::new(Opcode::Unreachable).with_type(self.types.void_id),
                );
            }
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
            call_insn.extra_mut().variadic_arg_start = variadic_arg_start;
            call_insn.extra_mut().ends_with_va_arg_pack = ends_with_va_arg_pack;
            call_insn.extra_mut().is_noreturn_call = is_noreturn_call;
            call_insn.extra_mut().callee_binding = binding;
            call_insn.extra_mut().known = known;
            call_insn.extra_mut().abi_info = Some(call_abi_info);
            self.emit(call_insn);
            if is_noreturn_call {
                self.emit_no_return(
                    Instruction::new(Opcode::Unreachable).with_type(self.types.void_id),
                );
            }
            result_sym
        }
    }

    /// Linearize a post-increment or post-decrement expression
    pub(crate) fn linearize_postop(&mut self, operand: &Expr, is_inc: bool) -> PseudoId {
        // `x++` on an atomic object is one read-modify-write, and its value is
        // the value *before* the operation -- exactly what fetch-add returns.
        if let Some(result) = self.try_emit_atomic_incdec(operand, is_inc, false) {
            return result;
        }

        // `E++` evaluates `E` exactly once (C17 6.5.2.4p2), so the address is
        // resolved here and serves both the read and the store-back. Reading
        // from the expression and then re-deriving the address for the store
        // ran every subexpression twice: `b[i++]++` incremented `i` twice and
        // updated the wrong element. `None` is a bare identifier, which has
        // no subexpressions to re-run.
        let place = self.resolve_rmw_place(operand);
        let typ = self.expr_type(operand);
        let val = match &place {
            Some(p) => self.load_rmw_place(p, typ),
            None => self.linearize_expr(operand),
        };
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
            self.pointer_step_bytes(operand, typ)
        } else if is_float {
            self.emit_fconst(FloatVal::from_f64(1.0), typ)
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
        if let Some(p) = &place {
            // Through the address the read came from. The postfix forms hand
            // back the value from *before* the update, which is already
            // narrowed for a bit-field because `emit_bitfield_load` produced
            // it -- so the store's answer is not needed here.
            self.store_rmw_place(p, final_result, typ);
            return old_val;
        }
        // Only a bare identifier reaches here: `resolve_rmw_place`
        // answers `Some` for every other lvalue and the branch above
        // stores through it. The arms that used to be here re-derived
        // an address that had already been computed, which is exactly
        // what ran the target a second time.
        if let ExprKind::Ident(symbol_id) = &operand.kind {
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

        old_val // Return old value
    }

    /// Linearize a binary expression (arithmetic, comparison, logical operators)
    /// Reduce an operation's result to the bit-field width it was computed at.
    ///
    /// C17 6.7.2.1p10 gives a bit-field a type of exactly its declared width,
    /// and 6.2.5p9 reduces an unsigned result modulo 2^width -- so `x.b << 32`
    /// with `unsigned long long b : 40` holding 0x100 is zero. The parser works
    /// out which operations carry a width and records it on the expression;
    /// this is where it is applied.
    ///
    /// `narrow_to_bitfield` already exists and does exactly the masking and
    /// sign-extension wanted, so this is the whole of the codegen side: no new
    /// opcode, no new instruction, no backend change.
    fn narrow_bitfield_result(&mut self, expr: &Expr, value: PseudoId) -> PseudoId {
        match expr.bitfield_bits {
            Some(bits) => {
                let typ = self.expr_type(expr);
                self.narrow_to_bitfield(value, bits, typ)
            }
            None => value,
        }
    }

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

            let scale = self.pointer_step_bytes(left, left_typ);
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

            let ptr_expr = if left_is_ptr_or_arr { left } else { right };
            let scale = self.pointer_step_bytes(ptr_expr, ptr_typ);
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
        } else if matches!(op, BinaryOp::Eq | BinaryOp::Ne)
            && (self.types.is_complex(left_typ) || self.types.is_complex(right_typ))
        {
            // Equality on complex operands compares both halves. The arm below
            // keys off the *result* type, which for a comparison is `int`, so
            // this fell through to the scalar path -- where `is_float` is
            // false for a complex type, so it emitted an integer compare over
            // a 128-bit operand and answered from the real half alone.
            let common = self.types.common_type(left_typ, right_typ);
            let left_addr = if self.types.is_complex(left_typ) {
                self.complex_operand_at_precision(left, common)
            } else {
                self.promote_real_to_complex(left, common)
            };
            let right_addr = if self.types.is_complex(right_typ) {
                self.complex_operand_at_precision(right, common)
            } else {
                self.promote_real_to_complex(right, common)
            };
            self.emit_complex_equality(op, left_addr, right_addr, common)
        } else if self.types.is_complex(result_typ) {
            // Complex arithmetic: expand to real/imaginary operations
            // For complex types, we need addresses to load real/imag parts
            // If an operand is not complex (e.g., real scalar), promote it
            let left_addr = if self.types.is_complex(left_typ) {
                self.complex_operand_at_precision(left, result_typ)
            } else {
                self.promote_real_to_complex(left, result_typ)
            };
            let right_addr = if self.types.is_complex(right_typ) {
                self.complex_operand_at_precision(right, result_typ)
            } else {
                self.promote_real_to_complex(right, result_typ)
            };
            self.emit_complex_binary(op, left_addr, right_addr, result_typ)
        } else {
            // For comparisons, compute common type for both operands
            // (usual arithmetic conversions)
            let operand_typ = if op.is_comparison() {
                self.types.common_type(left_typ, right_typ)
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

    /// The value of one half of `operand`: `__real__` and `__imag__` read as
    /// rvalues, and `creal` and `cimag`.
    ///
    /// A real operand is its own real half, and its imaginary half is a zero
    /// of its type -- gcc accepts both for `__real__` and `__imag__`. (The
    /// library functions' argument has already been converted to a complex
    /// type, so they never reach that case.)
    ///
    /// The zero is known without looking at the operand, but the operand is
    /// still *evaluated*: this is not an unevaluated context, so the effects
    /// in `__imag__ (x += 5.0)` have to happen.
    fn linearize_complex_half(&mut self, operand: &Expr, half: ComplexHalf) -> PseudoId {
        let op_typ = self.expr_type(operand);
        if !self.types.is_complex(op_typ) {
            return match half {
                ComplexHalf::Real => self.linearize_expr(operand),
                ComplexHalf::Imag => {
                    // The *value* is known in advance, the operand is not.
                    // `__imag__` is not an unevaluated context the way a
                    // `sizeof` operand is, so the effects still have to
                    // happen: returning the zero without linearizing the
                    // operand dropped them, and `__imag__ (x += 5.0)` left
                    // `x` alone. Only the value is discarded.
                    self.linearize_expr(operand);
                    if self.types.is_float(op_typ) {
                        self.emit_fconst(crate::float::FloatVal::ZERO, op_typ)
                    } else {
                        self.emit_const(0, op_typ)
                    }
                }
            };
        }
        let base_typ = self.types.complex_base(op_typ);
        let base_bits = self.types.size_bits(base_typ);
        let offset = match half {
            ComplexHalf::Real => 0,
            ComplexHalf::Imag => (base_bits / 8) as i64,
        };
        let addr = self.complex_operand_addr(operand);
        let value = self.alloc_pseudo();
        self.emit(Instruction::load(value, addr, offset, base_typ, base_bits));
        value
    }

    /// The libm opcode `op` of `args`, each already at `typ`, for a call
    /// that named `name`.
    fn linearize_libm(
        &mut self,
        op: Opcode,
        args: &[Expr],
        typ: TypeId,
        name: StringId,
    ) -> PseudoId {
        let arg_vals: Vec<PseudoId> = args.iter().map(|a| self.linearize_expr(a)).collect();
        self.emit_libm(op, &arg_vals, typ, name)
    }

    /// A library function's call evaluated in place: its arguments, already
    /// converted to the parameter types, and the computation the call stands
    /// for. Never an lvalue, so only ever reached for its value. `name` is
    /// the function the program called, and `narrowed` says whether it is
    /// computed by its `float` form instead.
    fn linearize_inline_library_call(
        &mut self,
        expr: &Expr,
        func: InlineLibraryFn,
        args: &[Expr],
        name: StringId,
        narrowed: Option<NarrowedLibraryCall>,
    ) -> PseudoId {
        let typ = self.expr_type(expr);
        if func.is_displaced(name, &self.defined_functions) {
            // The program's own function, defined below the call: gcc calls
            // it, and so does this. The arguments are at its parameter types
            // already, or -- narrowed -- at `float`, and then converted to
            // the call's own type, which is every parameter's for a function
            // that narrows.
            let arg_vals: Vec<(PseudoId, TypeId)> = args
                .iter()
                .map(|a| {
                    let val = self.linearize_expr(a);
                    match narrowed {
                        Some(n) => (self.emit_convert(val, n.typ, typ), typ),
                        None => (val, self.expr_type(a)),
                    }
                })
                .collect();
            let callee = self.library_function_name(self.strings.get(name));
            return self.emit_library_call(&callee, &arg_vals, typ);
        }
        match narrowed {
            Some(n) => {
                let val = self.compute_library_call(func, args, n.typ, n.name);
                self.emit_convert(val, n.typ, typ)
            }
            None => self.compute_library_call(func, args, typ, name),
        }
    }

    /// The computation a library call of `args` stands for, at `typ`; `name`
    /// is the library function that computes it, for a target or an
    /// argument that still needs the call.
    fn compute_library_call(
        &mut self,
        func: InlineLibraryFn,
        args: &[Expr],
        typ: TypeId,
        name: StringId,
    ) -> PseudoId {
        match (func, args) {
            (InlineLibraryFn::IntAbs, [arg]) => {
                let arg_val = self.linearize_expr(arg);
                let size = self.types.size_bits(typ);
                self.emit_int_abs(arg_val, typ, size)
            }
            (InlineLibraryFn::Fabs, [arg]) => {
                let arg_val = self.linearize_expr(arg);
                self.emit_fabs(arg_val, typ)
            }
            (InlineLibraryFn::CopySign, [x, y]) => {
                let x_val = self.linearize_expr(x);
                let y_val = self.linearize_expr(y);
                self.emit_copysign(x_val, y_val, typ)
            }
            (InlineLibraryFn::ComplexReal, [arg]) => {
                self.linearize_complex_half(arg, ComplexHalf::Real)
            }
            (InlineLibraryFn::ComplexImag, [arg]) => {
                self.linearize_complex_half(arg, ComplexHalf::Imag)
            }
            (InlineLibraryFn::Conjugate, [arg]) => self.emit_complex_conjugate(arg, typ),
            (InlineLibraryFn::Sqrt(errno), [arg]) => {
                let arg_val = self.linearize_expr(arg);
                self.emit_sqrt(arg_val, typ, name, errno)
            }
            (InlineLibraryFn::RoundToIntegral(how), [_]) => {
                self.linearize_libm(Opcode::RoundToIntegral(how), args, typ, name)
            }
            (InlineLibraryFn::FMin, [_, _]) => self.linearize_libm(Opcode::FMin, args, typ, name),
            (InlineLibraryFn::FMax, [_, _]) => self.linearize_libm(Opcode::FMax, args, typ, name),
            (InlineLibraryFn::Fma, [_, _, _]) => self.linearize_libm(Opcode::Fma, args, typ, name),
            (InlineLibraryFn::Memory(mem), [a, b, n]) => {
                let a = self.linearize_expr(a);
                let b = self.linearize_expr(b);
                let n = self.linearize_expr(n);
                self.emit_memory_fn(mem, [a, b, n])
            }
            _ => unreachable!(
                "{func:?} takes {} arguments, and the parser checked the call",
                func.arity()
            ),
        }
    }

    /// The block memory function `mem` of the arguments `args`, in the order
    /// the program wrote them, as the `Memcpy`, `Memset` or `Memmove` it
    /// performs. The instruction names the library function it calls when it
    /// is not expanded, which is its own and not always the program's:
    /// `mempcpy` copies with `memcpy`, `bcopy` moves with `memmove`. Answers
    /// the call's value.
    fn emit_memory_fn(&mut self, mem: MemoryFn, args: [PseudoId; 3]) -> PseudoId {
        let [a, b, n] = args;
        let (op, callee, dest, second) = match mem {
            MemoryFn::Copy | MemoryFn::CopyToEnd => (Opcode::Memcpy, "memcpy", a, b),
            MemoryFn::Set => (Opcode::Memset, "memset", a, b),
            MemoryFn::Move => (Opcode::Memmove, "memmove", a, b),
            MemoryFn::MoveSourceFirst => (Opcode::Memmove, "memmove", b, a),
        };
        let ptr = self.types.void_ptr_id;
        let result = self.alloc_pseudo();
        let callee = self.library_function_name(callee);
        self.emit(
            Instruction::new(op)
                .with_func(callee)
                .with_target(result)
                .with_src3(dest, second, n)
                .with_type_and_size(ptr, 64),
        );
        match mem {
            MemoryFn::CopyToEnd => {
                let end = self.alloc_pseudo();
                self.emit(Instruction::binop(Opcode::Add, end, dest, n, ptr, 64));
                end
            }
            // `bcopy` answers nothing: its `void` value is never read.
            MemoryFn::Copy | MemoryFn::Set | MemoryFn::Move | MemoryFn::MoveSourceFirst => result,
        }
    }

    /// Linearize a unary expression (prefix operators, address-of, dereference, etc.)
    pub(crate) fn linearize_unary(&mut self, expr: &Expr, op: UnaryOp, operand: &Expr) -> PseudoId {
        // Handle AddrOf specially - we need the lvalue address, not the value
        if op == UnaryOp::AddrOf {
            return self.linearize_lvalue(operand);
        }

        // `__real__` / `__imag__` read as a value; as an lvalue they are
        // handled by `linearize_lvalue`.
        if op == UnaryOp::Real {
            return self.linearize_complex_half(operand, ComplexHalf::Real);
        }
        if op == UnaryOp::Imag {
            return self.linearize_complex_half(operand, ComplexHalf::Imag);
        }

        // A complex value travels by address, so the scalar path below would
        // negate the address rather than the number it points at. `~` is the
        // GNU spelling of the conjugate, which negates only the imaginary
        // half; both halves are already in the same place.
        if op == UnaryOp::Neg || op == UnaryOp::BitNot {
            let typ = self.expr_type(expr);
            if self.types.is_complex(typ) {
                return if op == UnaryOp::Neg {
                    self.emit_complex_negate(operand, typ)
                } else {
                    self.emit_complex_conjugate(operand, typ)
                };
            }
        }

        // Handle PreInc/PreDec specially - they need store-back
        if op == UnaryOp::PreInc || op == UnaryOp::PreDec {
            // On an atomic object this is one read-modify-write whose value is
            // the *new* one (C17 6.5.3.1p2 defines ++E as E += 1).
            if let Some(result) = self.try_emit_atomic_incdec(operand, op == UnaryOp::PreInc, true)
            {
                return result;
            }

            // For deref operands like *s++, compute the lvalue address once
            // `++E` evaluates `E` exactly once (C17 6.5.3.1p2), so the
            // address is resolved here and serves both the read and the
            // store-back. This used to pre-compute the address for a `Deref`
            // target only -- the one shape someone had hit -- and every other
            // side-effecting target still ran twice: `++c[j++]` incremented
            // `j` twice and updated the wrong element. `None` is a bare
            // identifier, which has no subexpressions to re-run.
            let place = self.resolve_rmw_place(operand);
            let typ = self.expr_type(operand);
            let val = match &place {
                Some(p) => self.load_rmw_place(p, typ),
                None => self.linearize_expr(operand),
            };
            let is_float = self.types.is_float(typ);
            let is_ptr = self.types.kind(typ) == TypeKind::Pointer;

            // Compute new value - for pointers, scale by element size
            let increment = if is_ptr {
                self.pointer_step_bytes(operand, typ)
            } else if is_float {
                self.emit_fconst(FloatVal::from_f64(1.0), typ)
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
            if let Some(p) = &place {
                // Through the address the read came from.
                let narrowed = self.store_rmw_place(p, final_result, typ);
                // `++x.f` is `x.f += 1`, whose value is what the field now
                // holds (C17 6.5.16.1p2) -- so `signed int f : 3` at 3 gives
                // -4, not 4.
                return match narrowed {
                    Some((bit_width, field_typ)) => {
                        self.narrow_to_bitfield(final_result, bit_width, field_typ)
                    }
                    None => final_result,
                };
            }
            // Only a bare identifier reaches here: `resolve_rmw_place`
            // answers `Some` for every other lvalue and the branch above
            // stores through it. The arms that used to be here re-derived
            // an address that had already been computed, which is exactly
            // what ran the target a second time.
            if let ExprKind::Ident(symbol_id) = &operand.kind {
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

            // `++x.f` is `x.f += 1`, whose value is the value stored in the
            // field (C17 6.5.16.1p2) -- so `signed int f : 3` at 3 gives -4,
            // not 4. The postfix forms need no such care: they hand back the
            // value loaded before the update, which `emit_bitfield_load`
            // already narrowed.
            // A bare identifier is never a bit-field, so there is nothing
            // to reduce: the bit-field answer comes from `store_rmw_place`.
            return final_result;
        }

        // `!z` on a complex operand is `z == 0`, and a complex value is zero
        // only when both halves are. `emit_unary` compares a single value
        // against a single zero, which cannot express that, so the negation is
        // built from the shared truth-value conversion instead.
        let operand_typ = self.expr_type(operand);
        if op == UnaryOp::Not && self.types.is_complex(operand_typ) {
            let nonzero = self.emit_complex_nonzero(operand);
            let int_typ = self.types.int_id;
            return self.emit_unary(UnaryOp::Not, nonzero, int_typ);
        }

        let src = self.linearize_expr(operand);
        let result_typ = self.expr_type(expr);
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
        let name_str = self.symbol_name(symbol_id);

        // First check if it's an enum constant
        if sym.is_enum_constant() {
            if let Some(value) = sym.enum_value {
                // The symbol's own type, not `int`: an enumeration whose
                // members do not fit in `int` is wider than one, and emitting
                // its constants as `int` would truncate them right back.
                return self.emit_const(value, sym.typ);
            }
        }

        // Check if it's a local variable
        if let Some(local) = self.locals.get(&symbol_id).cloned() {
            // Check if this is a static local (sentinel value)
            if local.sym.0 == u32::MAX {
                // Static local - look up the global name and treat as global
                let key = format!("{}.{}", self.current_func_name, name_str);
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
            } else if type_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
                // va_list is defined as __va_list_tag[1] (an array type), so it decays to
                // a pointer when used in expressions (C99 6.3.2.1, 7.15.1). A target
                // whose va_list is itself a pointer falls through to the scalar load.
                if let Storage::Indirect(ptr_type) = local.storage {
                    // va_list parameter: local holds a pointer to the va_list struct
                    // Load the pointer value (array decay already happened at call site)
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
            if self.current_func_is_inline_definition && self.file_scope_statics.contains(&name_str)
            {
                if let Some(pos) = self.current_pos {
                    let msg = format!(
                        "inline definition of '{}' cannot reference file-scope static variable '{}'",
                        self.current_func_name, name_str
                    );
                    // gcc does not enforce this one, so real source contains
                    // it -- ffmpeg's `dv_guess_qnos` reads a file-scope
                    // `static const int` from an inline definition. It is
                    // relaxed by `-fpermissive`, which is where c17 keeps the
                    // constraints gcc lets through.
                    crate::diag::permissive_error(pos, &msg);
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
                || (type_kind == TypeKind::VaList && !self.types.va_list_is_pointer())
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

    /// Convert one arm of a conditional expression to the expression's type.
    ///
    /// C17 6.5.15p5 gives the whole expression the arms' common type, so *any*
    /// arm whose own type differs has to be converted -- a `float` arm feeding
    /// a `double` result as much as a narrower integer one.
    ///
    /// `emit_convert` already no-ops on an identical kind and size, so the only
    /// thing left to exclude is a type there is nothing to convert between --
    /// `void`, a struct, a pointer. Complex is excluded deliberately: widening
    /// it is a separate question from this one.
    /// One arm of a non-complex conditional, ready to merge: an aggregate's
    /// address, or a scalar converted to the result type.
    fn conditional_arm(
        &mut self,
        val: crate::ir::PseudoId,
        arm: &Expr,
        result_typ: crate::types::TypeId,
        aggregate: bool,
    ) -> crate::ir::PseudoId {
        if aggregate {
            return self.rvalue_addr(val, result_typ);
        }
        let arm_typ = self.expr_type(arm);
        self.convert_conditional_arm(val, arm_typ, result_typ)
    }

    fn convert_conditional_arm(
        &mut self,
        val: crate::ir::PseudoId,
        from: crate::types::TypeId,
        to: crate::types::TypeId,
    ) -> crate::ir::PseudoId {
        let convertible =
            |types: &crate::types::TypeTable, t| types.is_integer(t) || types.is_float(t);
        if from != to && convertible(self.types, from) && convertible(self.types, to) {
            self.emit_convert(val, from, to)
        } else {
            val
        }
    }

    /// One arm of a complex conditional, as the address of a `result_typ` value.
    ///
    /// The arms need not be complex themselves: `c ? 1 : z` has an `int` arm,
    /// and C17 6.5.15p5 converts it to the common type like any other operand.
    fn complex_arm_addr(&mut self, arm: &Expr, result_typ: TypeId) -> PseudoId {
        if self.types.is_complex(self.expr_type(arm)) {
            self.complex_operand_at_precision(arm, result_typ)
        } else {
            self.promote_real_to_complex(arm, result_typ)
        }
    }

    /// `c ? a : b` where the result is complex, merged **by address**.
    ///
    /// A complex value is two floats wide, so every other site in the
    /// linearizer passes one around as the address of its storage and reads
    /// the halves through that. The general conditional path does not: it
    /// phi-ed whatever `linearize_expr` returned, which for a complex operand
    /// is a `load` of the whole 128-bit object. Consumers then took those
    /// value bits for the address the convention promised and dereferenced
    /// them -- `creal(c ? z : w)` died on entirely valid code.
    ///
    /// Kept separate from `linearize_ternary`'s general path rather than
    /// folded into it: the phi here merges *pointers*, so it is a
    /// pointer-width phi of `pointer_to(result_typ)`, not a `size_bits`-wide
    /// phi of the complex type.
    fn linearize_complex_ternary(
        &mut self,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
        result_typ: TypeId,
    ) -> PseudoId {
        let cond_bool = self.linearize_condition(cond);
        let ptr_typ = self.types.pointer_to(result_typ);
        let ptr_bits = self.target.pointer_width;
        self.emit_diamond(
            cond_bool,
            ptr_typ,
            ptr_bits,
            |lin| lin.complex_arm_addr(then_expr, result_typ),
            |lin| lin.complex_arm_addr(else_expr, result_typ),
        )
    }

    pub(crate) fn linearize_ternary(
        &mut self,
        expr: &Expr,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
    ) -> PseudoId {
        // A constant condition selects one arm outright, and the other is
        // never evaluated (C17 6.5.15p4). Emitting it anyway is not merely
        // wasteful: glibc's `isinf` is
        //
        //     __builtin_types_compatible_p(__typeof(x), _Float128)
        //         ? __isinff128(x) : __builtin_isinf_sign(x)
        //
        // and emitting the untaken call left an undefined reference to
        // `__isinff128` in every object that used `isinf` on a double.
        //
        // Not when the untaken arm defines a label, though: a computed `goto`
        // can still reach it, so it has to be emitted. See
        // [`Expr::defines_label`].
        if let Some(holds) = self.constant_condition(cond) {
            let (taken, untaken) = if holds {
                (then_expr, else_expr)
            } else {
                (else_expr, then_expr)
            };
            if !untaken.defines_label() {
                return self.linearize_constant_arm(expr, taken);
            }
        }

        let result_typ = self.expr_type(expr);

        // A complex result travels by address, which neither path below can
        // produce -- so it gets its own.
        if self.types.is_complex(result_typ) {
            return self.linearize_complex_ternary(cond, then_expr, else_expr, result_typ);
        }

        // A struct or union too big for a register travels by address, so
        // its arms are merged as addresses: a pointer-sized select or phi of
        // `rvalue_addr`s. It was merged at the aggregate's own size -- a phi
        // of 128 bits or more over pointers. One that fits in a register
        // travels by value and is merged as one, below.
        let aggregate = matches!(
            self.types.kind(result_typ),
            TypeKind::Struct | TypeKind::Union
        ) && !self.aggregate_travels_by_value(result_typ);
        let (merge_typ, size) = if aggregate {
            (self.types.pointer_to(result_typ), self.target.pointer_width)
        } else if self.types.kind(result_typ) == TypeKind::Function {
            (result_typ, 64)
        } else {
            (result_typ, self.types.size_bits(result_typ))
        };

        if self.is_pure_expr(then_expr) && self.is_pure_expr(else_expr) && size <= 64 {
            // Pure: use Select instruction (enables cmov/csel)
            let cond_bool = self.linearize_condition(cond);
            let then_val = self.linearize_expr(then_expr);
            let else_val = self.linearize_expr(else_expr);
            let then_val = self.conditional_arm(then_val, then_expr, result_typ, aggregate);
            let else_val = self.conditional_arm(else_val, else_expr, result_typ, aggregate);

            let result = self.alloc_pseudo();
            self.emit(Instruction::select(
                result, cond_bool, then_val, else_val, merge_typ, size,
            ));
            result
        } else {
            // Impure: use control flow + phi for proper short-circuit evaluation.
            // Each arm is converted inside its own block, where it is the only
            // thing evaluated.
            let cond_bool = self.linearize_condition(cond);
            self.emit_diamond(
                cond_bool,
                merge_typ,
                size,
                |lin| {
                    let val = lin.linearize_expr(then_expr);
                    lin.conditional_arm(val, then_expr, result_typ, aggregate)
                },
                |lin| {
                    let val = lin.linearize_expr(else_expr);
                    lin.conditional_arm(val, else_expr, result_typ, aggregate)
                },
            )
        }
    }

    /// Lower GNU `a ?: b`.
    ///
    /// The whole point is that `a` is evaluated **once**: it supplies both the
    /// branch test and the value taken when it is true. Rewriting to
    /// `a ? a : b` in the parser would have called `f` twice in `f() ?: 0`.
    /// `a ?: b` where the *result* is complex, merged **by address**.
    ///
    /// Two constraints meet here. `a` is evaluated exactly once -- that is the
    /// whole point of the operator -- and a complex value travels by address,
    /// so for a complex `a` what is evaluated once is its *address*: it
    /// supplies the truth test through `emit_complex_nonzero_at` and,
    /// converted to the result's precision, the value taken when `a` is
    /// nonzero. Going back to the `Expr` for either would evaluate `f()` twice
    /// in `f() ?: 0`.
    ///
    /// `a` itself need not be complex. Only the *result* is, and it is complex
    /// as soon as either operand is: `d ?: z` has a `double` left operand and a
    /// `double _Complex` result. Taking a real `a`'s address as though it were
    /// a complex object read the neighbouring stack slot as the imaginary half
    /// -- and for an rvalue, `rvalue_addr` hands back the value's own bits, so
    /// `g() ?: z` dereferenced a `double` as a pointer.
    fn linearize_complex_elvis(
        &mut self,
        cond: &Expr,
        else_expr: &Expr,
        cond_typ: TypeId,
        result_typ: TypeId,
    ) -> PseudoId {
        // The single evaluation, before any branch. A complex operand is
        // carried by address; a real one is an ordinary value, and converting
        // it is left to the true arm so nothing sits between the comparison
        // and the branch that consumes it.
        let cond_complex = self.types.is_complex(cond_typ);
        let evaluated = if cond_complex {
            self.complex_operand_addr(cond)
        } else {
            self.linearize_expr(cond)
        };
        let cond_bool = if cond_complex {
            self.emit_complex_nonzero_at(evaluated, cond_typ)
        } else {
            self.emit_compare_zero(evaluated, cond_typ)
        };

        let ptr_typ = self.types.pointer_to(result_typ);
        let ptr_bits = self.target.pointer_width;
        self.emit_diamond(
            cond_bool,
            ptr_typ,
            ptr_bits,
            |lin| {
                if cond_complex {
                    lin.complex_addr_at_precision(evaluated, cond_typ, result_typ)
                } else {
                    lin.promote_real_value_to_complex(evaluated, cond_typ, result_typ)
                }
            },
            |lin| lin.complex_arm_addr(else_expr, result_typ),
        )
    }

    /// The value of a conditional expression whose constant condition
    /// selected `taken`, converted to the conditional's own type.
    fn linearize_constant_arm(&mut self, expr: &Expr, taken: &Expr) -> PseudoId {
        let result_typ = self.expr_type(expr);
        if self.types.is_complex(result_typ) {
            return self.complex_arm_addr(taken, result_typ);
        }
        let value = self.linearize_expr(taken);
        let taken_typ = self.expr_type(taken);
        self.emit_convert(value, taken_typ, result_typ)
    }

    pub(crate) fn linearize_elvis(
        &mut self,
        expr: &Expr,
        cond: &Expr,
        else_expr: &Expr,
    ) -> PseudoId {
        let result_typ = self.expr_type(expr);
        let cond_typ = self.expr_type(cond);

        // A constant condition picks one side outright and never evaluates the
        // other, for the reason `linearize_ternary` records.
        // The condition is evaluated either way, so only `else_expr` can be
        // the untaken arm whose label keeps it alive.
        if let Some(cond_const) = self.eval_const_expr(cond) {
            if cond_const == 0 {
                return self.linearize_constant_arm(expr, else_expr);
            }
            if !else_expr.defines_label() {
                return self.linearize_constant_arm(expr, cond);
            }
        }

        // A complex result travels by address, and the left operand is both
        // the truth test and the true value -- see `linearize_complex_elvis`.
        if self.types.is_complex(result_typ) {
            return self.linearize_complex_elvis(cond, else_expr, cond_typ, result_typ);
        }

        let size = if self.types.kind(result_typ) == TypeKind::Function {
            64
        } else {
            self.types.size_bits(result_typ)
        };

        // Evaluated here, before either form below, so there is exactly one
        // evaluation on every path.
        let cond_val = self.linearize_expr(cond);
        let cond_bool = self.emit_compare_zero(cond_val, cond_typ);

        if self.is_pure_expr(else_expr) && size <= 64 {
            // No branch here, so both arms are converted in place.
            let then_val = self.convert_conditional_arm(cond_val, cond_typ, result_typ);
            let mut else_val = self.linearize_expr(else_expr);
            let else_typ = self.expr_type(else_expr);
            else_val = self.convert_conditional_arm(else_val, else_typ, result_typ);
            let result = self.alloc_pseudo();
            self.emit(Instruction::select(
                result, cond_bool, then_val, else_val, result_typ, size,
            ));
            return result;
        }

        // Impure right-hand side: it must not be evaluated when the condition
        // is true, so it needs its own block.
        //
        // The true value is the condition, converted to the result type -- done
        // *inside* the true block, where the ternary also converts its arms.
        // Converting before the `cbr` is equally correct as IR, and reads more
        // naturally since the value is already in hand, but it puts an
        // instruction between the comparison and the branch that consumes it,
        // and the aarch64 backend then emits a branch on the wrong register.
        // That is a backend defect and is reported as one; this is not the
        // place to depend on it.
        self.emit_diamond(
            cond_bool,
            result_typ,
            size,
            |lin| lin.convert_conditional_arm(cond_val, cond_typ, result_typ),
            |lin| {
                let val = lin.linearize_expr(else_expr);
                let else_typ = lin.expr_type(else_expr);
                lin.convert_conditional_arm(val, else_typ, result_typ)
            },
        )
    }

    /// Lower `__builtin_clrsb` and its wider siblings.
    ///
    /// The count of redundant sign bits is `clz` of the value with its sign
    /// folded away, less one -- but `clz(0)` is undefined, and `x` of 0 or -1
    /// folds to exactly that. c17 and gcc happen to answer differently there,
    /// so relying on it would be relying on undefined behaviour agreeing.
    ///
    /// Instead the folded value is shifted up one and given a low bit:
    ///
    /// ```text
    /// clrsb(x) = clz(((x ^ (x >> (W-1))) << 1) | 1)
    /// ```
    ///
    /// The shift absorbs the `- 1`, the set bit makes the input nonzero for
    /// every `x`, and the fold's result always has its top bit clear so the
    /// shift cannot lose information. `x` is evaluated once, which is why this
    /// is a node rather than a rewrite over `__builtin_clz`.
    fn linearize_clrsb(&mut self, arg: &Expr, width: u32) -> PseudoId {
        let int_id = self.types.int_id;
        // The sign-extracting shift must be signed; everything after it is bit
        // manipulation and must be *unsigned*. `folded << 1` overflows a
        // signed type whenever the top data bit is set -- `clrsb(INT_MIN)`
        // folds to 0x7fffffff, and shifting that left is undefined -- so at
        // -O2 the folder was free to answer 30 where -O0 answered 0.
        let signed_typ = if width == 32 {
            self.types.int_id
        } else {
            self.types.long_id
        };
        let val_typ = if width == 32 {
            self.types.uint_id
        } else {
            self.types.ulong_id
        };
        let val = self.linearize_expr(arg);

        // x >> (W-1): all ones when negative, zero when not.
        let shift = self.emit_const((width - 1) as i128, int_id);
        let sign = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Asr,
            sign,
            val,
            shift,
            signed_typ,
            width,
        ));

        // x ^ sign: x when non-negative, ~x when negative. Top bit is clear.
        let folded = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Xor,
            folded,
            val,
            sign,
            val_typ,
            width,
        ));

        // (folded << 1) | 1 -- never zero, and one bit shorter to count.
        let one = self.emit_const(1, int_id);
        let shifted = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Shl,
            shifted,
            folded,
            one,
            val_typ,
            width,
        ));
        let one_again = self.emit_const(1, val_typ);
        let nonzero = self.alloc_pseudo();
        self.emit(Instruction::binop(
            Opcode::Or,
            nonzero,
            shifted,
            one_again,
            val_typ,
            width,
        ));

        let result = self.alloc_pseudo();
        let op = if width == 32 {
            Opcode::Clz32
        } else {
            Opcode::Clz64
        };
        self.emit(
            Instruction::new(op)
                .with_target(result)
                .with_src(nonzero)
                .with_size(width)
                .with_type(int_id),
        );
        result
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
                    func.add_local(&unique_name, sym_id, *typ, self.current_bb, None);
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
            // Variadic function support (va_* builtins)
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
                let arg_size = self.types.size_bits(*arg_type);

                // Every aggregate gets a local of its own, whatever its size,
                // and the backend writes the argument into it -- the same
                // arrangement a call returning an aggregate uses, and for the
                // same reason.
                //
                // The size did once gate this, on the crate-wide convention
                // that an aggregate pseudo holds its value below eight bytes
                // and its address at or above. `emit_assign`'s struct path
                // does not honour that convention: it asks `linearize_lvalue`
                // for an address, and `rvalue_addr` hands back any non-`Sym`
                // pseudo unchanged, taking it for a pointer already. So
                // `x = va_arg(ap, struct tiny)` copied from whatever address
                // the struct's own four bytes spelled. Giving the result a
                // `Sym` makes `rvalue_addr` take its address instead, which
                // is what the small-struct return path does with `__sret1_`.
                //
                // A complex value is addressed at every size, so it takes the
                // same local. Given a bare pseudo instead, the backend wrote
                // the value's bytes into a register every consumer then
                // dereferenced as the address of the two halves.
                let result = if self.types.is_aggregate_or_complex(*arg_type) {
                    self.frame_temp("__vaarg", *arg_type)
                } else {
                    self.alloc_pseudo()
                };

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
            // Byte-swapping builtins
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

            // Count trailing zeros builtins
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

            // Count leading zeros builtins
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

            ExprKind::Clrsb { arg } => self.linearize_clrsb(arg, 32),
            ExprKind::Clrsbl { arg } | ExprKind::Clrsbll { arg } => self.linearize_clrsb(arg, 64),

            // Population count builtins
            // `__builtin_popcount` counts an `unsigned int`, `popcountl` and
            // `popcountll` a 64-bit value; the count is an `int` either way.
            ExprKind::Popcount { arg }
            | ExprKind::Popcountl { arg }
            | ExprKind::Popcountll { arg } => {
                let (op, operand) = if matches!(expr.kind, ExprKind::Popcount { .. }) {
                    (Opcode::Popcount32, self.types.uint_id)
                } else {
                    (Opcode::Popcount64, self.types.ulonglong_id)
                };
                let arg_val = self.linearize_expr(arg);
                let result = self.alloc_pseudo();
                let int = self.types.int_id;
                let mut insn =
                    Instruction::unop(op, result, arg_val, int, self.types.size_bits(int));
                insn.src_typ = Some(operand);
                insn.src_size = self.types.size_bits(operand);
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

            ExprKind::FpTest { test, arg } => self.linearize_fp_test(*test, arg),
            ExprKind::FpCompare { cmp, lhs, rhs } => self.linearize_fp_compare(*cmp, lhs, rhs),

            ExprKind::FpClassify { classes, arg } => self.linearize_fp_classify(classes, arg),

            ExprKind::Unreachable => {
                // __builtin_unreachable() - marks code path as never reached
                // Emits an instruction that will trap if actually executed
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Unreachable)
                    .with_target(result)
                    .with_type(self.types.void_id);
                self.emit_no_return(insn);
                result
            }

            ExprKind::FrameAddress { level } => {
                let result = self.alloc_pseudo();
                let insn = Instruction::frame_walk(
                    Opcode::FrameAddress,
                    result,
                    *level,
                    self.types.void_ptr_id,
                );
                self.emit(insn);
                result
            }

            ExprKind::ReturnAddress { level } => {
                let result = self.alloc_pseudo();
                let insn = Instruction::frame_walk(
                    Opcode::ReturnAddress,
                    result,
                    *level,
                    self.types.void_ptr_id,
                );
                self.emit(insn);
                result
            }

            ExprKind::Setjmp { env } => {
                // setjmp(env) - saves execution context, returns int
                let env_val = self.linearize_expr(env);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Setjmp)
                    .with_func(self.library_function_name("setjmp"))
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

                let mut insn = Instruction::new(Opcode::Longjmp)
                    .with_func(self.library_function_name("longjmp"));
                insn.target = Some(result);
                insn.src = vec![env_val, val_val];
                insn.typ = Some(self.types.void_id);
                self.emit_no_return(insn);
                result
            }
            _ => unreachable!(),
        }
    }

    /// Build one `__c11_atomic_*` builtin from its already-parsed operands.
    ///
    /// Every builtin except the compare-exchange pair and the fences has the
    /// same shape: linearize the pointer, take the pointee's type and width,
    /// and emit `op [ptr, value?, order]`. Eleven hand-written copies of that
    /// is how the operand convention drifted out of the linearizer's reach in
    /// the first place -- `AsmConstraint::is_memory` had the same problem.
    /// gcc's `__atomic_*` / `__sync_*` read-modify-write.
    ///
    /// The operation itself goes through the same `emit_atomic_rmw` the
    /// compound assignment on an `_Atomic` object uses, so a native
    /// fetch-and-op is taken where the target has one and the CAS loop
    /// elsewhere -- one implementation of the LL/SC rules, not two.
    ///
    /// `*_and_fetch` re-applies the operation to the value the exchange
    /// returned. That is arithmetic on a value already in hand, not a second
    /// access to the object, and it reuses the operand *pseudo* -- so
    /// `__sync_add_and_fetch(p, f())` calls `f` exactly once.
    fn linearize_gnu_atomic_rmw(
        &mut self,
        op: GnuAtomicOp,
        ptr: &Expr,
        val: &Expr,
        order: &Expr,
        returns_new: bool,
    ) -> PseudoId {
        let ptr_type = self.expr_type(ptr);
        let elem_typ = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
        let bits = self.types.size_bits(elem_typ);

        let addr = self.linearize_expr(ptr);
        let value_typ = self.expr_type(val);
        let raw = self.linearize_expr(val);
        // Pointer arithmetic scales by the element size, as it does for `+=`.
        let is_ptr_arith = self.types.kind(elem_typ) == TypeKind::Pointer
            && self.types.is_integer(value_typ)
            && matches!(op, GnuAtomicOp::Add | GnuAtomicOp::Sub);
        let (operand, operand_typ) = if is_ptr_arith {
            (
                self.scale_pointer_addend(elem_typ, value_typ, raw),
                self.types.long_id,
            )
        } else {
            // Unlike an operator, a builtin converts its value argument to the
            // object's type itself (gcc documents the parameter as that type),
            // so there is no common type left to compute at.
            (self.emit_convert(raw, value_typ, elem_typ), elem_typ)
        };
        // The order argument is accepted and evaluated, as gcc evaluates it,
        // but every lowering here is sequentially consistent: `emit_atomic_rmw`
        // and its CAS loop are, and answering a weaker order with a stronger
        // one is always correct.
        let _ = self.linearize_expr(order);

        let lv = AtomicLvalue {
            addr,
            elem_typ,
            size_bits: bits,
        };

        // Each builtin is the compound assignment of the same name, so it
        // goes through the same model: `nand` is the one that has no operator
        // spelling, and it is `&` with the result complemented before it
        // converts back to the object's type (`CompoundAssign::invert`).
        let assign_op = match op {
            GnuAtomicOp::Add => AssignOp::AddAssign,
            GnuAtomicOp::Sub => AssignOp::SubAssign,
            GnuAtomicOp::And | GnuAtomicOp::Nand => AssignOp::AndAssign,
            GnuAtomicOp::Or => AssignOp::OrAssign,
            GnuAtomicOp::Xor => AssignOp::XorAssign,
        };
        let ca = CompoundAssign {
            is_ptr_arith,
            invert: op == GnuAtomicOp::Nand,
            ..CompoundAssign::new(assign_op, elem_typ, operand_typ)
        };

        let old = self.emit_atomic_rmw(&lv, &ca, operand);
        if !returns_new {
            return old;
        }
        // The new value, recomputed from the old one rather than read back:
        // arithmetic on a value in hand is not a second access to the object.
        self.compound_assign_value(&ca, old, operand)
    }

    /// `__sync_bool_compare_and_swap` and `__sync_val_compare_and_swap`.
    ///
    /// The expected value arrives by value, so it is staged in a temporary
    /// whose address the compare-exchange takes. Afterwards that temporary
    /// holds the object's old value either way: on success the object held
    /// what was expected, and on failure both backends write the observed
    /// value back through the pointer.
    fn linearize_gnu_atomic_cas(
        &mut self,
        ptr: &Expr,
        expected: &Expr,
        desired: &Expr,
        returns_old: bool,
    ) -> PseudoId {
        let ptr_type = self.expr_type(ptr);
        let elem_typ = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
        let bits = self.types.size_bits(elem_typ);

        let addr = self.linearize_expr(ptr);
        let exp_typ = self.expr_type(expected);
        let exp_raw = self.linearize_expr(expected);
        let exp_val = self.emit_convert(exp_raw, exp_typ, elem_typ);
        let des_typ = self.expr_type(desired);
        let des_raw = self.linearize_expr(desired);
        let des_val = self.emit_convert(des_raw, des_typ, elem_typ);

        let exp_addr = self.frame_temp_addr("__casexp", elem_typ);
        self.emit(Instruction::store(exp_val, exp_addr, 0, elem_typ, bits));

        let ok = self.alloc_reg_pseudo();
        let order = self.emit_const(MemoryOrder::SeqCst as i128, self.types.int_id);
        let mut cas = Instruction::new(Opcode::AtomicCas).with_target(ok);
        cas.src = vec![addr, exp_addr, des_val, order];
        cas.typ = Some(self.types.bool_id);
        cas.size = bits;
        cas.extra_mut().memory_order = MemoryOrder::SeqCst;
        self.emit(cas);

        if !returns_old {
            return ok;
        }
        let old = self.alloc_reg_pseudo();
        self.emit(Instruction::load(old, exp_addr, 0, elem_typ, bits));
        old
    }

    fn emit_c11_atomic_builtin(
        &mut self,
        op: Opcode,
        ptr: &Expr,
        val: Option<&Expr>,
        order: Option<&Expr>,
    ) -> PseudoId {
        let ptr_val = self.linearize_expr(ptr);
        let value = val.map(|v| self.linearize_expr(v));

        // `atomic_init` carries no order argument: it is a non-atomic store,
        // so it is emitted as a relaxed one.
        let (order_val, memory_order) = match order {
            Some(o) => (self.linearize_expr(o), self.eval_memory_order(o)),
            None => (
                self.emit_const(MemoryOrder::Relaxed as i128, self.types.int_id),
                MemoryOrder::Relaxed,
            ),
        };

        let ptr_type = self.expr_type(ptr);
        let elem_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
        let size = self.types.size_bits(elem_type);
        let result = self.alloc_pseudo();

        let mut insn = Instruction::new(op).with_target(result).with_src(ptr_val);
        if let Some(v) = value {
            insn = insn.with_src(v);
        }
        insn = insn
            .with_src(order_val)
            .with_type_and_size(elem_type, size)
            .with_memory_order(memory_order);
        self.emit(insn);
        result
    }

    pub(crate) fn linearize_c11_atomic(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
            ExprKind::C11AtomicInit { ptr, val } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicStore, ptr, Some(val), None)
            }

            ExprKind::C11AtomicLoad { ptr, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicLoad, ptr, None, Some(order))
            }

            ExprKind::C11AtomicStore { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicStore, ptr, Some(val), Some(order))
            }

            ExprKind::C11AtomicExchange { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicSwap, ptr, Some(val), Some(order))
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

            ExprKind::GnuAtomicRmw {
                op,
                ptr,
                val,
                order,
                returns_new,
            } => self.linearize_gnu_atomic_rmw(*op, ptr, val, order, *returns_new),

            ExprKind::GnuAtomicCas {
                ptr,
                expected,
                desired,
                returns_old,
            } => self.linearize_gnu_atomic_cas(ptr, expected, desired, *returns_old),

            ExprKind::C11AtomicFetchAdd { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicFetchAdd, ptr, Some(val), Some(order))
            }

            ExprKind::C11AtomicFetchSub { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicFetchSub, ptr, Some(val), Some(order))
            }

            ExprKind::C11AtomicFetchAnd { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicFetchAnd, ptr, Some(val), Some(order))
            }

            ExprKind::C11AtomicFetchOr { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicFetchOr, ptr, Some(val), Some(order))
            }

            ExprKind::C11AtomicFetchXor { ptr, val, order } => {
                self.emit_c11_atomic_builtin(Opcode::AtomicFetchXor, ptr, Some(val), Some(order))
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

    /// `__builtin_*_overflow` where the destination is 128 bits wide.
    ///
    /// The ordinary lowering computes in a type twice the destination's width
    /// and asks whether narrowing lost anything. Nothing here is wider than
    /// 128 bits, so at that width it compared a value to itself and always
    /// answered "no overflow". The result has to be examined directly instead,
    /// with the classic per-operation predicates.
    ///
    /// Operands arrive already converted to the destination type. Where that
    /// conversion is not value-preserving -- a negative operand with an
    /// unsigned destination, or an `unsigned __int128` one with a signed
    /// destination -- the answer follows the converted value rather than the
    /// mathematical one.
    fn linearize_checked_arith_wide(
        &mut self,
        op: crate::parse::ast::CheckedOp,
        a: PseudoId,
        b: PseudoId,
        addr: PseudoId,
        dst: TypeId,
    ) -> PseudoId {
        use crate::parse::ast::CheckedOp;
        let bin = match op {
            CheckedOp::Add => BinaryOp::Add,
            CheckedOp::Sub => BinaryOp::Sub,
            CheckedOp::Mul => BinaryOp::Mul,
        };
        let r = self.emit_binary(bin, a, b, dst, dst);
        let dst_size = self.types.size_bits(dst);
        self.emit(Instruction::store(r, addr, 0, dst, dst_size));

        let int_id = self.types.int_id;
        let unsigned = self.types.is_unsigned(dst);

        match (op, unsigned) {
            // The sum wrapped exactly when it came out below an addend.
            (CheckedOp::Add, true) => self.emit_binary(BinaryOp::Lt, r, a, int_id, dst),
            // A difference is representable exactly when it is not negative.
            (CheckedOp::Sub, true) => self.emit_binary(BinaryOp::Lt, a, b, int_id, dst),
            // Signed addition overflows when both addends differ in sign from
            // the result, which is what `(a^r) & (b^r)` being negative says.
            (CheckedOp::Add, false) | (CheckedOp::Sub, false) => {
                // Addition: `(a^r) & (b^r)` -- both addends differ in sign
                // from the sum. Subtraction: `(a^b) & (a^r)` -- the operands
                // differ in sign and the result differs from the minuend.
                let (p, q) = match op {
                    CheckedOp::Add => (b, r),
                    _ => (b, r),
                };
                let x1 = match op {
                    CheckedOp::Add => self.emit_binary(BinaryOp::BitXor, a, q, dst, dst),
                    _ => self.emit_binary(BinaryOp::BitXor, a, p, dst, dst),
                };
                let x2 = match op {
                    CheckedOp::Add => self.emit_binary(BinaryOp::BitXor, p, q, dst, dst),
                    _ => self.emit_binary(BinaryOp::BitXor, a, q, dst, dst),
                };
                let both = self.emit_binary(BinaryOp::BitAnd, x1, x2, dst, dst);
                let zero = self.emit_const(0, dst);
                self.emit_binary(BinaryOp::Lt, both, zero, int_id, dst)
            }
            // Multiplication is checked on magnitudes, in unsigned arithmetic
            // that cannot itself overflow: |a| exceeds the largest multiplier
            // that still fits exactly when it is above `limit / |b|`. The
            // limit depends on the sign of the product, since the negative
            // range holds one more value than the positive one.
            (CheckedOp::Mul, _) => {
                let u = self.types.uint128_id;
                let (ua, ub, limit) = if unsigned {
                    let max = self.emit_const(-1i128, u);
                    (a, b, max)
                } else {
                    // `(x ^ (x >> 127)) - (x >> 127)` is |x| as a bit pattern,
                    // exact for the most negative value too.
                    let sh = self.emit_const(127, self.types.int_id);
                    let ma = self.emit_binary(BinaryOp::Shr, a, sh, dst, dst);
                    let mb = self.emit_binary(BinaryOp::Shr, b, sh, dst, dst);
                    let xa = self.emit_binary(BinaryOp::BitXor, a, ma, dst, dst);
                    let xb = self.emit_binary(BinaryOp::BitXor, b, mb, dst, dst);
                    let aa = self.emit_binary(BinaryOp::Sub, xa, ma, dst, dst);
                    let ab = self.emit_binary(BinaryOp::Sub, xb, mb, dst, dst);
                    let ua = self.emit_convert(aa, dst, u);
                    let ub = self.emit_convert(ab, dst, u);
                    // Signs differ: the product may reach 2^127, one past the
                    // largest positive value.
                    let sx = self.emit_binary(BinaryOp::BitXor, a, b, dst, dst);
                    let sm = self.emit_binary(BinaryOp::Shr, sx, sh, dst, dst);
                    let smu = self.emit_convert(sm, dst, u);
                    let one = self.emit_const(1, u);
                    let neg = self.emit_binary(BinaryOp::BitAnd, smu, one, u, u);
                    let smax = self.emit_const(i128::MAX, u);
                    let limit = self.emit_binary(BinaryOp::Add, smax, neg, u, u);
                    (ua, ub, limit)
                };
                // Zero multiplies never overflow, and dividing by the zero
                // would trap, so the divisor is nudged to one and the whole
                // answer gated on it.
                let zero = self.emit_const(0, u);
                let nz = self.emit_binary(BinaryOp::Ne, ub, zero, int_id, u);
                let nzu = self.emit_convert(nz, int_id, u);
                let one = self.emit_const(1, u);
                let bump = self.emit_binary(BinaryOp::BitXor, nzu, one, u, u);
                let d = self.emit_binary(BinaryOp::Add, ub, bump, u, u);
                let q = self.emit_binary(BinaryOp::Div, limit, d, u, u);
                let over = self.emit_binary(BinaryOp::Gt, ua, q, int_id, u);
                self.emit_binary(BinaryOp::BitAnd, over, nz, int_id, int_id)
            }
        }
    }

    /// `__builtin_*_overflow`: compute exactly, store the wrapped result, and
    /// answer whether wrapping lost anything.
    fn linearize_checked_arith(
        &mut self,
        op: &crate::parse::ast::CheckedOp,
        a: &Expr,
        b: &Expr,
        res: &Expr,
        store: bool,
    ) -> PseudoId {
        let a_typ = self.expr_type(a);
        let b_typ = self.expr_type(b);
        let res_typ = self.expr_type(res);
        // The `_p` forms name the destination type with a value rather than a
        // pointer to one, and store nothing -- so the type to ask about is
        // `res`'s own, not its pointee's.
        let dst_typ = if store {
            self.types.base_type(res_typ).unwrap_or(self.types.int_id)
        } else {
            res_typ
        };

        let all_unsigned = self.types.is_unsigned(a_typ)
            && self.types.is_unsigned(b_typ)
            && self.types.is_unsigned(dst_typ);
        // A difference can be negative even when every operand is
        // unsigned, and negative is exactly the unrepresentable case
        // for an unsigned destination. Computing it in an unsigned
        // `wide` wraps it instead, and then nothing downstream can see
        // it: `exact < 0` is never true in unsigned arithmetic, and
        // with `wide == dst_typ` the narrow fast path reports a hard
        // "no overflow". `__builtin_sub_overflow(0u, 1u, &u128)`
        // answered 0 where gcc answers 1.
        //
        // A sum or product of unsigned operands still needs the
        // unsigned range: two 64-bit values can exceed the signed
        // 128-bit maximum. A difference cannot -- it needs one bit
        // more than the wider operand, and both are under 128 here --
        // so signing it costs nothing.
        let subtracting = matches!(op, crate::parse::ast::CheckedOp::Sub);
        let wide = if all_unsigned && !subtracting {
            self.types.uint128_id
        } else {
            self.types.int128_id
        };

        let av = self.linearize_expr(a);
        let bv = self.linearize_expr(b);
        // Evaluated either way. For the storing forms this is the address to
        // write through; for the `_p` forms the value is discarded, but gcc
        // still evaluates the argument -- `__builtin_add_overflow_p(1, 1, i++)`
        // increments `i` -- and a side effect that happens under gcc and not
        // here is exactly the kind of difference that goes unnoticed until it
        // changes a result.
        let addr = self.linearize_expr(res);

        let aw = self.emit_convert(av, a_typ, wide);
        let bw = self.emit_convert(bv, b_typ, wide);
        let bin = match op {
            crate::parse::ast::CheckedOp::Add => BinaryOp::Add,
            crate::parse::ast::CheckedOp::Sub => BinaryOp::Sub,
            crate::parse::ast::CheckedOp::Mul => BinaryOp::Mul,
        };
        if self.types.size_bits(dst_typ) >= 128 {
            // A 128-bit destination has no wider type to compute in,
            // so "did narrowing lose anything" would compare a value
            // to itself -- `linearize_checked_arith_wide` below
            // examines the result directly instead. Where the
            // computation in `wide` is nevertheless exact, the check
            // becomes "is the exact value representable at all". The
            // two differ for a negative operand with an unsigned
            // destination: `__builtin_add_overflow(-1, 5u, &u128)` is
            // 4, not 2^128-1 + 5.
            //
            // Exactness is not "narrower than the destination":
            // `(u64)-1 * (u64)-1` is just under 2^128, which fits
            // `unsigned __int128` but *not* `__int128`, so the exact
            // computation would itself wrap. Count magnitude bits --
            // `w` for an unsigned operand, `w-1` for a signed one --
            // and require the result to fit `wide`.
            let magnitude_bits = |t: TypeId| {
                let w = self.types.size_bits(t);
                if self.types.is_unsigned(t) {
                    w
                } else {
                    w - 1
                }
            };
            let (ma, mb) = (magnitude_bits(a_typ), magnitude_bits(b_typ));
            let room = if self.types.is_unsigned(wide) {
                128
            } else {
                127
            };
            let exact_fits = match op {
                crate::parse::ast::CheckedOp::Mul => ma + mb <= room,
                // A sum or difference needs one bit more than the wider
                // operand, i.e. `ma.max(mb) + 1 <= room`.
                _ => ma.max(mb) < room,
            };
            if self.types.size_bits(a_typ) < 128 && self.types.size_bits(b_typ) < 128 && exact_fits
            {
                let exact = self.emit_binary(bin, aw, bw, wide, wide);
                let dst_size = self.types.size_bits(dst_typ);
                let narrowed = self.emit_convert(exact, wide, dst_typ);
                if store {
                    self.emit(Instruction::store(narrowed, addr, 0, dst_typ, dst_size));
                }
                // `wide` is unsigned only when the destination is too,
                // so the two agree except when a signed exact value has
                // to land in an unsigned destination -- where the
                // negatives are precisely the unrepresentable ones.
                return if wide == dst_typ {
                    self.emit_const(0, self.types.int_id)
                } else {
                    let zero = self.emit_const(0, wide);
                    self.emit_binary(BinaryOp::Lt, exact, zero, self.types.int_id, wide)
                };
            }
            let a128 = self.emit_convert(av, a_typ, dst_typ);
            let b128 = self.emit_convert(bv, b_typ, dst_typ);
            return self.linearize_checked_arith_wide(*op, a128, b128, addr, dst_typ);
        }

        let exact = self.emit_binary(bin, aw, bw, wide, wide);

        let narrowed = self.emit_convert(exact, wide, dst_typ);
        let dst_size = self.types.size_bits(dst_typ);
        if store {
            self.emit(Instruction::store(narrowed, addr, 0, dst_typ, dst_size));
        }

        let back = self.emit_convert(narrowed, dst_typ, wide);
        self.emit_binary(BinaryOp::Ne, exact, back, self.types.int_id, wide)
    }

    /// `sizeof expr`.  Folds to a constant unless the operand is a VLA, whose
    /// size is only known at run time.
    fn linearize_sizeof_expr(&mut self, inner_expr: &Expr) -> PseudoId {
        // `sizeof(p[i++])` on a pointer to a VLA steps `i`, as gcc does.
        if self.sizeof_evaluates(inner_expr) {
            self.linearize_expr(inner_expr);
        }
        // Check if this is a VLA variable - need runtime sizeof
        if let ExprKind::Ident(symbol_id) = &inner_expr.kind {
            if let Some(info) = self.locals.get(symbol_id).cloned() {
                if let (Some(size_sym), Some(elem_type)) = (info.vla_size_sym, info.vla_elem_type) {
                    // VLA: compute sizeof at runtime as num_elements * sizeof(element)
                    let result_typ = self.types.ulong_id;
                    let elem_size = self.types.size_bytes(elem_type) as i64;

                    // Load the stored number of elements
                    let num_elements = self.alloc_pseudo();
                    let load_insn = Instruction::load(num_elements, size_sym, 0, result_typ, 64);
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

        // `sizeof(a[0])` on a variably-modified array: the row size is
        // only known at run time, and the type reports 0.
        if let Some(size) = self.vm_sizeof_expr(inner_expr) {
            return size;
        }

        // Non-VLA: compute size at compile time
        let inner_typ = self.expr_type(inner_expr);
        let size = self.types.size_bytes(inner_typ);
        // sizeof returns size_t, which is unsigned long in our implementation
        let result_typ = self.types.ulong_id;
        self.emit_const(size as i128, result_typ)
    }

    /// GNU `&&label`: the address of a label, for a computed goto.
    fn linearize_label_addr(&mut self, name: &StringId, expr: &Expr) -> PseudoId {
        let Some(sym) = self.take_label_address(*name, expr.pos) else {
            return self.emit_const(0, self.types.void_ptr_id);
        };
        let sym_pseudo = self.alloc_pseudo();
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(Pseudo::sym(sym_pseudo, sym));
        }
        let dst = self.alloc_pseudo();
        let void_ptr = self.types.void_ptr_id;
        self.emit(Instruction::sym_addr(dst, sym_pseudo, void_ptr));
        dst
    }

    /// `offsetof(type, member)`: the constant [`crate::constexpr::offset_of`]
    /// computes. The parser has already rejected a path naming nothing.
    fn linearize_offsetof(&mut self, type_id: &TypeId, path: &[OffsetOfPath]) -> PseudoId {
        let offset = crate::constexpr::offset_of(self, *type_id, path)
            .expect("offsetof: the parser accepted a path that names no member");
        self.emit_const(offset, self.types.ulong_id)
    }

    /// The recorded extent of a variably-modified typedef, one level in.
    fn linearize_vm_typedef_extent(&mut self, symbol_id: &SymbolId, level: &u32) -> PseudoId {
        let dim = self
            .vm_typedef_dims
            .get(symbol_id)
            .and_then(|dims| dims.get(*level as usize))
            .copied();
        self.load_vm_extent(dim)
    }

    /// One extent of an object expression's type, read from the hidden
    /// locals its object's declaration stored: that of its `level`-th unsized
    /// array level, which is how [`crate::parse::ast::vm_extent_count`]
    /// counts them. An incomplete `[]` among them reads as 0.
    fn linearize_vm_object_extent(&mut self, object: &Expr, level: u32) -> PseudoId {
        let typ = self.expr_type(object);
        let array = match self.types.kind(typ) {
            TypeKind::Pointer => self.types.base_type(typ).unwrap_or(typ),
            _ => typ,
        };
        let mut unsized_at = Vec::new();
        let mut cur = array;
        while self.types.kind(cur) == TypeKind::Array {
            unsized_at.push(self.types.get(cur).array_size.is_none());
            cur = self.types.base_type(cur).unwrap_or(self.types.int_id);
        }
        let position = unsized_at
            .iter()
            .enumerate()
            .filter(|(_, is_unsized)| **is_unsized)
            .nth(level as usize)
            .map(|(i, _)| i);
        let dim = position.and_then(|i| {
            self.vm_type_extents(object)
                .and_then(|(dims, _)| dims.get(i).copied())
        });
        self.load_vm_extent(dim)
    }

    /// Read back an extent recorded by `record_vm_extents`.
    fn load_vm_extent(&mut self, dim: Option<VmDim>) -> PseudoId {
        let ulong = self.types.ulong_id;
        match dim {
            Some(VmDim::Const(n)) => self.emit_const(n as i128, ulong),
            Some(VmDim::Sym(sym)) => {
                let loaded = self.alloc_pseudo();
                self.emit(Instruction::load(loaded, sym, 0, ulong, 64));
                loaded
            }
            // The declaration that records an extent -- a typedef's, or
            // the object's -- is always linearized before any use of it
            // can be: a use is in its scope, and scope begins at the
            // declarator. Nothing measurable is left to do if that ever
            // fails to hold.
            None => self.emit_const(0, ulong),
        }
    }

    /// `__builtin_complex(real, imag)`.
    fn linearize_builtin_complex(&mut self, real: &Expr, imag: &Expr, expr: &Expr) -> PseudoId {
        // __builtin_complex(real, imag) - construct complex value
        let complex_typ = self.expr_type(expr);
        let base_typ = self.types.complex_base(complex_typ);
        let base_bits = self.types.size_bits(base_typ);
        let base_bytes = (base_bits / 8) as i64;

        let real_val = self.linearize_expr(real);
        let imag_val = self.linearize_expr(imag);

        // Allocate local to hold the complex value, return its address
        let result = self.frame_temp_addr("__ctmp", complex_typ);
        self.emit(Instruction::store(real_val, result, 0, base_typ, base_bits));
        self.emit(Instruction::store(
            imag_val, result, base_bytes, base_typ, base_bits,
        ));
        result
    }

    pub(crate) fn linearize_expr(&mut self, expr: &Expr) -> PseudoId {
        // Set current position for debug info
        self.current_pos = Some(expr.pos);

        // Every rvalue read of an `_Atomic` object is itself an atomic
        // operation (C17 6.7.3). This is the single funnel for reads, so
        // branching here covers initializers, conditions, call arguments and
        // operands alike. On x86 a plain aligned load already is sequentially
        // consistent, but on aarch64 this is the difference between `ldr` and
        // `ldar`.
        if let Some(lv) = self.atomic_lvalue(expr) {
            return self.emit_atomic_load(&lv);
        }

        match &expr.kind {
            // `__builtin_va_arg_pack()` is not a value: it stands for the
            // caller's argument list, and `linearize_call` lifts it off the
            // argument list into a flag on the call. Reaching here means it
            // was written somewhere no argument list could carry it.
            ExprKind::VaArgPack => {
                crate::diag::error(
                    expr.pos,
                    "'__builtin_va_arg_pack' may only appear as the last argument of a call",
                );
                self.emit_const(0, self.types.int_id)
            }
            // `__builtin_constant_p`, for an operand the parser could not
            // fold. gcc answers it after optimization, so it is deferred to
            // `sccp` -- and to `ir::lower`, which answers 0 for whatever is
            // left, including everything at `-O0`.
            //
            // The builtin does not evaluate its argument, so an operand with
            // side effects is answered 0 outright rather than linearized. A
            // pure one costs nothing: its computation is dead once the
            // placeholder folds, and `dce` collects it.
            ExprKind::ConstantP(inner) => {
                if !self.is_pure_expr(inner) {
                    return self.emit_const(0, self.types.int_id);
                }
                let operand = self.linearize_expr(inner);
                let result = self.alloc_reg_pseudo();
                self.emit(
                    Instruction::new(Opcode::ConstantP)
                        .with_target(result)
                        .with_src(operand)
                        .with_type_and_size(self.types.int_id, 32),
                );
                result
            }
            // Resolved when the enclosing function is inlined, since it counts
            // the *caller's* arguments. A leftover is diagnosed after the
            // inliner runs, by `opt::check_forwarding_resolved`.
            ExprKind::VaArgPackLen => {
                let result = self.alloc_reg_pseudo();
                self.emit(
                    Instruction::new(Opcode::VaArgPackLen)
                        .with_target(result)
                        .with_type_and_size(self.types.int_id, 32),
                );
                result
            }
            // GNU `&&label`. The block already emits an assembly label of
            // exactly this spelling -- `Label::name()` -- and both backends
            // already lower a leading-`.` global to a pc-relative address, so
            // this needs no opcode of its own.
            ExprKind::LabelAddr(name) => self.linearize_label_addr(name, expr),

            // One extent of a variably modified `typedef`, evaluated when the
            // typedef's declaration was reached and stored in a hidden local
            // since (C17 6.7.7p3). Reading it back here is what keeps
            // `typedef int T[n]; n = 100; T a;` giving `a` the extent `n` had
            // at the typedef.
            ExprKind::VmTypedefExtent(symbol_id, level) => {
                self.linearize_vm_typedef_extent(symbol_id, level)
            }
            // One extent of `typeof(v)`: `v`'s, as its declaration recorded
            // it, so later changes to what sized `v` do not reach it.
            ExprKind::VmObjectExtent(object, level) => {
                self.linearize_vm_object_extent(object, *level)
            }
            // A value of variably modified type written as a type-name: its
            // extents first, then the value.
            ExprKind::VmTypeName { symbol, dims, expr } => {
                self.record_type_name_extents(*symbol, self.expr_type(expr), dims);
                self.linearize_expr(expr)
            }

            // `__builtin_add_overflow(a, b, res)` and its siblings.
            //
            // Computed in 128 bits, which holds every exact sum, difference
            // and product of two operands of 64 bits or fewer, then narrowed
            // to the destination's type and widened back: if the round trip
            // changes the value, the operation overflowed. That is the
            // definition, and it needs no new opcode on either target -- the
            // flag-reading forms the hardware offers would.
            //
            // Signedness of the wide type follows the operands: a product of
            // two 64-bit unsigned values can exceed the signed 128-bit range,
            // so all-unsigned operands compute unsigned.
            ExprKind::CheckedArith {
                op,
                a,
                b,
                res,
                store,
            } => self.linearize_checked_arith(op, a, b, res, *store),

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
                self.emit_const(*c as i128, typ)
            }

            ExprKind::StringLit(s) => {
                let label = self.module.add_string(s.clone());
                self.emit_string_sym(expr, label)
            }

            ExprKind::Utf16StringLit(u) => {
                let label = self.module.add_utf16_string(u.clone());
                self.emit_string_sym(expr, label)
            }

            ExprKind::WideStringLit(u) | ExprKind::Utf32StringLit(u) => {
                let label = self.module.add_utf32_string(u.clone());
                self.emit_string_sym(expr, label)
            }

            ExprKind::Ident(symbol_id) => self.linearize_ident(expr, *symbol_id),

            ExprKind::FuncName => self.linearize_func_name(),

            ExprKind::Unary { op, operand } => {
                let v = self.linearize_unary(expr, *op, operand);
                self.narrow_bitfield_result(expr, v)
            }

            ExprKind::Binary { op, left, right } => {
                let v = self.linearize_binary(expr, *op, left, right);
                self.narrow_bitfield_result(expr, v)
            }

            ExprKind::Assign { op, target, value } => self.emit_assign(*op, target, value),

            ExprKind::PostInc(operand) => self.linearize_postop(operand, true),

            ExprKind::PostDec(operand) => self.linearize_postop(operand, false),

            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => self.linearize_ternary(expr, cond, then_expr, else_expr),

            ExprKind::CondElvis { cond, else_expr } => self.linearize_elvis(expr, cond, else_expr),

            ExprKind::Call {
                func,
                args,
                binding,
                known,
            } => self.linearize_call(expr, func, args, *binding, *known),

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

            ExprKind::SizeofType(typ, dims) => {
                // sizeof returns size_t, which is unsigned long in our implementation
                let result_typ = self.types.ulong_id;

                // C17 6.5.3.4p2: for a variable length array type the operand
                // is evaluated and the size computed at run time.
                // `record_vm_extents` pairs one size expression with each
                // absent extent -- the same pairing a declared `int a[n][4][m]`
                // goes through -- and spills each to a hidden local, so the
                // expression is linearized exactly once however many times the
                // product reads it back.
                if crate::parse::ast::sizeof_type_is_runtime(self.types, *typ, dims) {
                    let (extents, elem) = self.record_vm_extents(*typ, dims, "sizeof");
                    if let Some(size) = self.vm_extent_size(&extents, elem) {
                        return size;
                    }
                }

                let size = self.types.size_bytes(*typ);
                self.emit_const(size as i128, result_typ)
            }

            ExprKind::SizeofExpr(inner_expr) => self.linearize_sizeof_expr(inner_expr),

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
            | ExprKind::Clrsb { .. }
            | ExprKind::Clrsbl { .. }
            | ExprKind::Clrsbll { .. }
            | ExprKind::Popcount { .. }
            | ExprKind::Popcountl { .. }
            | ExprKind::Popcountll { .. }
            | ExprKind::Alloca { .. }
            | ExprKind::FpTest { .. }
            | ExprKind::FpCompare { .. }
            | ExprKind::FpClassify { .. }
            | ExprKind::Unreachable
            | ExprKind::FrameAddress { .. }
            | ExprKind::ReturnAddress { .. }
            | ExprKind::Setjmp { .. }
            | ExprKind::Longjmp { .. } => self.linearize_builtin(expr),

            ExprKind::InlineLibraryCall {
                func,
                args,
                name,
                narrowed,
            } => self.linearize_inline_library_call(expr, *func, args, *name, *narrowed),

            ExprKind::OffsetOf { type_id, path } => self.linearize_offsetof(type_id, path),

            ExprKind::C11AtomicInit { .. }
            | ExprKind::C11AtomicLoad { .. }
            | ExprKind::GnuAtomicRmw { .. }
            | ExprKind::GnuAtomicCas { .. }
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
                // It is a block, so it is a declaration scope like any other:
                // its declarations do not outlive it, and the storage of a
                // VLA declared in it goes back when it ends. Without the
                // scope, `for (...) (void)({ int a[n]; ... });` allocated
                // every time round and released nothing.
                let scope = self.push_scope();
                // Linearize all the statements first
                for item in stmts {
                    match item {
                        BlockItem::Declaration(decl) => self.linearize_local_decl(decl),
                        BlockItem::Statement(s) => self.linearize_stmt(s),
                    }
                }
                // The result is the value of the final expression, computed
                // before the scope ends: it may read the VLA being released.
                let value = self.linearize_expr(result);
                self.pop_scope(scope);
                value
            }

            ExprKind::BuiltinComplex { real, imag } => {
                self.linearize_builtin_complex(real, imag, expr)
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

// Public API

pub fn linearize(
    tu: &TranslationUnit,
    symbols: &SymbolTable,
    types: &TypeTable,
    strings: &StringTable,
    target: &Target,
    debug: bool,
    trapping_math: bool,
) -> Module {
    let mut linearizer = Linearizer::new(symbols, types, strings, target);
    linearizer.trapping_math = trapping_math;
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
