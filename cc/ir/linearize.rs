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
use super::memexpand::BlockOp;
use super::ssa::ssa_convert;
use super::{
    conversion_raises_fp, BasicBlock, BasicBlockId, CallAbiInfo, FenceScope, FpRaise, Function,
    Initializer, Instruction, MemoryOrder, Module, Opcode, Pseudo, PseudoId, PseudoKind,
};
use crate::abi::{get_abi_for_conv, CallingConv};
use crate::diag::{get_all_stream_names, Position};
use crate::float::FloatVal;
use crate::ir::linearize_atomic::{AtomicLvalue, OrderedAccess};
use crate::ir::linearize_emit::CompoundAssign;
use crate::ir::linearize_stmt::SwitchCtx;
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

const DEFAULT_LOCALS_CAPACITY: usize = 64;
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

/// A vector parameter that arrives as its aggregate carrier, or by
/// reference; see [`Linearizer::store_vector_memory_params`].
struct VectorMemoryParam {
    name: String,
    symbol: Option<SymbolId>,
    vector: TypeId,
    carrier: TypeId,
    arg: PseudoId,
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

/// What a block-scope name stands for, which decides how it is read,
/// stored and addressed.
#[derive(Clone)]
pub(crate) enum LocalBinding {
    /// An object in this function's frame: `sym` is its stack slot.
    Frame { sym: PseudoId, storage: Storage },
    /// A block-scope `static`: an object with static storage duration,
    /// emitted as a global under `global`, a name of the form
    /// `funcname.varname.N` no other declaration can spell.
    Static { global: String },
    /// No object at all: the extents of a type name's variably modified
    /// type, recorded under a symbol no identifier names (see
    /// [`Linearizer::record_type_name_extents`]).
    ExtentsOnly,
}

/// Information about a local variable
#[derive(Clone)]
pub(crate) struct LocalVarInfo {
    /// What the name stands for.
    pub(crate) binding: LocalBinding,
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
}

impl LocalVarInfo {
    /// A name bound to `binding`, with no variably modified extents.
    pub(crate) fn new(binding: LocalBinding, typ: TypeId) -> Self {
        Self {
            binding,
            typ,
            vla_size_sym: None,
            vla_elem_type: None,
            vla_outer_extent: None,
            vm_row_dims: vec![],
        }
    }

    /// An object that lives in its stack slot `sym`.
    pub(crate) fn frame(sym: PseudoId, typ: TypeId) -> Self {
        Self::new(
            LocalBinding::Frame {
                sym,
                storage: Storage::InSlot,
            },
            typ,
        )
    }

    /// The stack slot, for a name bound to one.
    pub(crate) fn frame_sym(&self) -> Option<PseudoId> {
        match self.binding {
            LocalBinding::Frame { sym, .. } => Some(sym),
            LocalBinding::Static { .. } | LocalBinding::ExtentsOnly => None,
        }
    }
}

/// Where an object an expression designates lives, for
/// [`Linearizer::read_object`].
#[derive(Clone, Copy)]
pub(crate) enum ObjectPlace {
    /// A symbol's own storage: a local's slot, a static, a global, a
    /// compound literal.
    Sym(PseudoId),
    /// `offset` bytes past an address computed at run time.
    At(PseudoId, i64),
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
                crate::types::own_bit_bytes(self.offset, bit_offset, bit_width)
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

/// Where an initializer of an object with static storage duration is, inside
/// that object -- what [`Linearizer::admit_fam_visits`] needs to decide whether
/// a flexible array member may be initialized there.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct StaticInitNesting {
    /// Inside a subobject, not at the object's own top level.
    pub(crate) nested: bool,
    /// Inside an element of an array.
    pub(crate) in_array: bool,
}

/// The storage duration of the object an initializer list initializes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InitStorage {
    /// Initialized by stores each time its declaration is reached.
    Automatic,
    /// Laid out as a data image, at the given nesting.
    Static(StaticInitNesting),
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

impl StructFieldVisit {
    /// This field's placement, if it is a bit-field of non-zero width.
    pub(crate) fn bitfield(&self) -> Option<crate::types::Bitfield> {
        crate::types::Bitfield::from_parts(
            self.offset,
            self.bit_offset,
            self.bit_width,
            self.access_bytes,
        )
    }
}

pub(crate) enum StructFieldVisitKind {
    /// A single expression to initialize this field
    Expr(Box<Expr>),
    /// Sub-elements from brace elision
    BraceElision(Vec<InitElement>),
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
    /// The `cleanups` depth on entry; every cleanup above it belongs to a
    /// variable this scope declares, and runs when it ends.
    pub(crate) cleanup_entry: usize,
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

/// What a call calls.
enum CallTarget {
    /// A function, by name.
    Direct(String),
    /// The function pointer the callee expression evaluated to.
    Indirect(PseudoId),
}

/// What a callee's type says about a call to it: what lowering the
/// arguments and building the call instruction both read.
struct CalleeSignature {
    /// The convention that classifies the return, the arguments taken by
    /// reference, and the registers.
    conv: CallingConv,
    /// The prototype's parameter types; `None` without a prototype.
    params: Option<Vec<TypeId>>,
    /// A function type with no prototype (C17 6.5.2.2p6).
    unprototyped: bool,
    /// Where the variadic arguments start, for a variadic callee.
    variadic_arg_start: Option<usize>,
    /// The callee does not return.
    is_noreturn: bool,
}

/// A call's lowered arguments: the values and, in step, the types the ABI
/// classifies them by.
struct CallArgs {
    vals: Vec<PseudoId>,
    types: Vec<TypeId>,
}

/// What a call instruction records beyond its operands.
struct CallFacts {
    variadic_arg_start: Option<usize>,
    ends_with_va_arg_pack: bool,
    is_noreturn: bool,
    binding: crate::parse::ast::CalleeBinding,
    known: Option<crate::parse::ast::LibFn>,
    abi_info: Box<CallAbiInfo>,
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
    /// The `switch` statements whose bodies are being lowered, innermost
    /// last. A `case` or `default` label belongs to the innermost one.
    pub(crate) switch_stack: Vec<SwitchCtx>,
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
    /// The current function's return type when it is a GNU vector, which
    /// `return` converts to the vector's carrier.
    pub(crate) vector_return: Option<TypeId>,
    /// Current function name (for generating unique static local names)
    pub(crate) current_func_name: String,
    /// The function's name as written, which `__func__` holds; the emitted
    /// name above is an asm label's when it has one.
    pub(crate) current_func_ident: StringId,

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
    /// The `cleanup(fn)` calls of the variables in scope, outermost first.
    /// See [`super::linearize_cleanup`].
    pub(crate) cleanups: Vec<super::linearize_cleanup::PendingCleanup>,
    /// For each label, the variables with a cleanup in whose scope it lies,
    /// outermost first: a `goto` runs the cleanup of every variable in scope
    /// at the jump and not at its label. Taken from the jump-scope walk,
    /// because a forward `goto` is lowered before its label.
    pub(crate) label_cleanups: std::collections::HashMap<StringId, Vec<SymbolId>>,
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

    /// Where the static initializer being lowered is inside its object. Each
    /// level of [`Self::ast_init_list_to_ir`] sets it for the levels below and
    /// restores it, so it reads as the top level between objects.
    pub(crate) static_init_nesting: StaticInitNesting,

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
            locals: HashMap::with_capacity(DEFAULT_LOCALS_CAPACITY),
            vm_typedef_dims: HashMap::new(),
            label_map: HashMap::with_capacity(DEFAULT_LABEL_MAP_CAPACITY),
            break_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            continue_targets: Vec::with_capacity(DEFAULT_LOOP_DEPTH_CAPACITY),
            switch_stack: Vec::new(),
            run_ssa: true, // Enable SSA conversion by default
            symbols,
            types,
            strings,
            struct_return_ptr: None,
            reg_aggregate_return_type: None,
            vector_return: None,
            current_func_name: String::new(),
            current_func_ident: StringId::EMPTY,
            addr_taken_labels: Vec::new(),
            label_refs: Vec::new(),
            defined_labels: std::collections::HashSet::new(),
            written_labels: std::collections::HashSet::new(),
            label_vla_depth: std::collections::HashMap::new(),
            pending_goto_vla: Vec::new(),
            vla_marks: Vec::new(),
            cleanups: Vec::new(),
            label_cleanups: std::collections::HashMap::new(),
            volatile_init_object: None,
            static_init_nesting: StaticInitNesting::default(),
            func_has_vla: false,
            indirect_dispatch: None,
            static_local_counter: 0,
            compound_literal_counter: 0,
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
            cleanup_entry: self.cleanups.len(),
        }
    }

    /// Leave the scope `scope` opened: run the cleanups of the variables
    /// declared in it, release its VLAs and restore every local it shadowed.
    ///
    /// The cleanups come first, while every variable they name is still in
    /// scope and still has its storage; then the stack restore, while the
    /// block the scope ends in is still the current one. Both are emitted
    /// only on the falling-out path -- a `break`, `continue`, `goto` or
    /// `return` that left already did its own unwinding and terminated the
    /// block.
    pub(crate) fn pop_scope(&mut self, scope: Scope) {
        self.close_cleanup_scope(&scope);
        self.close_vla_scope(&scope);
        self.end_lifetimes();
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

    /// Mark the end of the lifetime of every local the innermost scope
    /// declared, on the path that falls out of it.
    ///
    /// Only that path: a `break`, `goto` or `return` that leaves the scope
    /// has terminated its block, and an object whose end is not marked on
    /// some path just stays live longer there. That is the safe direction --
    /// the allocator may then share its slot less, never more.
    ///
    /// Not for the function's own scope, the outermost: what it declares
    /// lives until a return, and it is closed only after SSA has run.
    fn end_lifetimes(&mut self) {
        if self.local_scope_stack.len() < 2 || self.is_terminated() || self.current_bb.is_none() {
            return;
        }
        let Some(entries) = self.local_scope_stack.last() else {
            return;
        };
        // A block-scope `static` or `extern` is in scope here too, and names
        // a global: only a frame slot has a lifetime to end.
        let Some(func) = self.current_func.as_ref() else {
            return;
        };
        let ending: Vec<PseudoId> = entries
            .iter()
            .filter_map(|(sym, _)| self.locals.get(sym)?.frame_sym())
            .filter(|&p| func.local_of(p).is_some())
            .collect();
        for local in ending {
            self.emit(Instruction::lifetime_end(local));
        }
    }

    /// Insert a local variable, recording the previous value for scope restoration.
    pub(crate) fn insert_local(&mut self, sym: SymbolId, info: LocalVarInfo) {
        let prev = self.locals.insert(sym, info);
        if let Some(scope) = self.local_scope_stack.last_mut() {
            scope.push((sym, prev));
        }
    }

    /// The symbol an identifier names when it names one: a static local's
    /// global, or the identifier itself for a file-scope or `extern` name.
    /// `None` for a frame local, which a static initializer can neither read
    /// nor take the address of, and for a type name's extents.
    pub(crate) fn global_name_of(&self, symbol_id: SymbolId) -> Option<String> {
        match self.locals.get(&symbol_id).map(|local| &local.binding) {
            None => Some(self.symbol_name(symbol_id)),
            Some(LocalBinding::Static { global }) => Some(global.clone()),
            Some(LocalBinding::Frame { .. } | LocalBinding::ExtentsOnly) => None,
        }
    }

    /// A `Sym` pseudo naming the assembler symbol `name`: a global, a string
    /// literal's label, a code label. A frame local's is made by
    /// [`Self::named_local`].
    pub(crate) fn sym_pseudo(&mut self, name: String) -> PseudoId {
        let sym_id = self.alloc_pseudo();
        let pseudo = Pseudo::sym(sym_id, name);
        if let Some(func) = &mut self.current_func {
            func.add_pseudo(pseudo);
        }
        sym_id
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

    /// The width of a pointer: an address, and a saved stack pointer.
    pub(crate) fn ptr_bits(&self) -> u32 {
        self.types.size_bits(self.types.void_ptr_id)
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
            16 => self.types.uint128_id,
            _ => self
                .types
                .unsigned_of_size(storage_size)
                .unwrap_or(self.types.uint_id),
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

    /// Keep a memory access's constant offset inside a machine displacement.
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
        if !insn.op.addresses_memory() || i32::try_from(insn.offset).is_ok() {
            return insn;
        }
        let Some(&base) = insn.src.first() else {
            return insn;
        };
        let base = self.rvalue_addr(base, self.types.char_id);
        insn.src[0] = self.offset_address(base, insn.offset);
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
        // Same type and size - no conversion needed
        if self.types.kind(from_typ) == self.types.kind(to_typ)
            && self.types.size_bits(from_typ) == self.types.size_bits(to_typ)
        {
            return val;
        }

        // An array or a function is converted as the pointer it decays to
        // (C17 6.3.2.1p3-4), which is what its value already is: the address.
        let from_typ = self.types.decayed_value(from_typ);
        let from_size = self.types.size_bits(from_typ);
        let to_size = self.types.size_bits(to_typ);
        let from_float = self.types.is_float(from_typ);
        let to_float = self.types.is_float(to_typ);
        let from_kind = self.types.kind(from_typ);
        let to_kind = self.types.kind(to_typ);

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
    /// (`Function::remove_unreachable_blocks`). Appending it to the block the
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
    /// `Function::remove_unreachable_blocks` takes it away again with everything
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
    ///
    /// A scalar does exactly the same: none ever does under System V or
    /// AAPCS64, while the Microsoft x64 convention returns `long double` and
    /// `__float128` through the pointer, since no register position is wide
    /// enough for either.
    ///
    /// `conv` is the convention of the function whose return this is: the
    /// callee's at a call, the function's own in its definition.
    pub(crate) fn returns_via_hidden_pointer(&self, typ: TypeId, conv: CallingConv) -> bool {
        let kind = self.types.kind(typ);
        let abi = get_abi_for_conv(conv, self.target);
        if self.types.is_complex(typ) || self.types.is_scalar(typ) {
            return matches!(
                abi.classify_return(typ, self.types),
                crate::abi::ArgClass::Indirect { .. }
            );
        }
        if kind != TypeKind::Struct && kind != TypeKind::Union {
            return false;
        }
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
            self.named_local(local_sym, name, ptr_type, None, None);
            let ptr_size = self.types.size_bits(ptr_type);
            self.emit(Instruction::store(
                arg_pseudo, local_sym, 0, ptr_type, ptr_size,
            ));
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(
                    symbol_id,
                    LocalVarInfo::new(
                        LocalBinding::Frame {
                            sym: local_sym,
                            // va_list param: the slot holds a pointer to the
                            // caller's va_list, array decay having happened
                            // at the call site.
                            storage: Storage::Indirect(ptr_type),
                        },
                        typ, // Keep original va_list type for type checking
                    ),
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
            self.named_local(local_sym, name, typ, None, None);
            self.fill_struct_param(local_sym, typ, arg_pseudo);

            // Register as a local variable (only if named parameter)
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(symbol_id, LocalVarInfo::frame(local_sym, typ));
            }
        }
    }

    /// A vector parameter that arrives as an aggregate carrier's bytes, or
    /// by reference: its local is the vector, filled as a parameter of the
    /// carrier type would be.
    fn store_vector_memory_params(&mut self, params: Vec<VectorMemoryParam>) {
        for p in params {
            let local_sym = self.alloc_pseudo();
            self.named_local(local_sym, p.name, p.vector, None, None);
            self.fill_struct_param(local_sym, p.carrier, p.arg);
            if let Some(symbol_id) = p.symbol {
                self.insert_local(symbol_id, LocalVarInfo::frame(local_sym, p.vector));
            }
        }
    }

    /// Fill `local_sym` from the parameter `arg_pseudo` of the struct, union
    /// or by-reference type `typ`, which arrived as the convention passes it.
    fn fill_struct_param(&mut self, local_sym: PseudoId, typ: TypeId, arg_pseudo: PseudoId) {
        let typ_size = self.types.size_bits(typ);
        // The copy length is an object size, so it is counted in bytes.
        // `typ_size` saturates for an aggregate past `u32::MAX` bits and is
        // good only for the class tests below, which compare against 64.
        let typ_bytes = self.types.size_bytes(typ) as i64;
        let conv = self.current_calling_conv;
        let abi = get_abi_for_conv(conv, self.target);
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
        //
        // A convention that passes such a value by reference instead --
        // AAPCS64, Win64 -- hands over a pointer to the caller's copy,
        // which the pointer path below copies out of.
        let arrived_by_value = !abi.indirect_param_is_reference()
            && crate::arch::lir::memory_class_bytes(self.types, typ).is_some();
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
        } else if typ_size > 64 || self.passed_by_reference(typ, conv) {
            // Medium struct (9-16 bytes): arg_pseudo is a pointer (current behavior).
            // So is anything passed by reference, at any size: a Win64
            // three-byte struct or `long double`.
            // Copy each 8-byte chunk through pointer dereference.
            let vol = BlockVolatility {
                dst: self.types.contains_volatile(typ),
                src: false,
            };
            self.emit_block_copy(local_sym, arg_pseudo, typ_bytes, vol);
        } else if typ_bytes > 0 {
            // Small struct: arg_pseudo contains the value directly. A
            // complex value passed that way (Win64) is its bits, stored
            // as the integer they are. A zero-sized one arrives in
            // nothing (`ArgClass::Ignore`) and has nothing to store.
            let as_typ = if self.types.is_complex(typ) {
                self.types
                    .unsigned_of_size(typ_bytes as usize)
                    .unwrap_or(typ)
            } else {
                typ
            };
            self.emit(Instruction::store(
                arg_pseudo, local_sym, 0, as_typ, typ_size,
            ));
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
            self.named_local(local_sym, name, typ, None, None);
            let typ_size_bytes = self.types.size_bytes(typ);
            if let Some(func) = &mut self.current_func {
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
                self.insert_local(symbol_id, LocalVarInfo::frame(local_sym, typ));
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
            self.named_local(local_sym, name, typ, None, None);

            // Store the incoming argument value to the local, converted from
            // its promoted type when an identifier list declared it.
            // A vector's carrier holds its bits, which are stored as they are.
            let (arg_pseudo, stored) = if passed_as == typ || self.types.is_vector(typ) {
                (arg, passed_as)
            } else {
                (self.emit_convert(arg, passed_as, typ), typ)
            };
            let typ_size = self.types.size_bits(stored);
            self.emit(Instruction::store(
                arg_pseudo, local_sym, 0, stored, typ_size,
            ));

            // Register as a local variable for name lookup (only if named parameter)
            if let Some(symbol_id) = symbol_id_opt {
                self.insert_local(symbol_id, LocalVarInfo::frame(local_sym, typ));
            }
        }
    }

    /// Clear the per-function state carried on the linearizer.
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
        self.locals.clear();
        self.local_scope_stack.clear();
        self.label_map.clear();
        self.break_targets.clear();
        self.continue_targets.clear();
        self.switch_stack.clear();
        self.struct_return_ptr = None;
        self.reg_aggregate_return_type = None;
        self.vector_return = None;
        self.current_func_name = self.emitted_name(func.name);
        self.current_func_ident = func.name;
        self.addr_taken_labels.clear();
        self.label_refs.clear();
        self.defined_labels.clear();
        self.label_vla_depth.clear();
        self.pending_goto_vla.clear();
        self.vla_marks.clear();
        self.cleanups.clear();
        self.func_has_vla = Self::declares_vla(&func.body);
        self.indirect_dispatch = None;
        // Remove from extern_symbols since we're defining this function
        self.module.extern_symbols.remove(&self.current_func_name);

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
            | crate::parse::ast::Stmt::Labeled { stmt: body, .. } => Self::declares_vla(body),
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
        let labels = self.check_jumps_into_protected_scopes(&func.body);

        let func_scope = self.reset_for_function(func);
        self.written_labels = labels.written;
        self.label_cleanups = labels.cleanups;

        // Create function - use storage class from FunctionDef
        let modifiers = self.types.modifiers(func.return_type);
        let is_static = func.is_static;
        let is_inline = func.is_inline;
        let is_extern = modifiers.contains(TypeModifiers::EXTERN);
        let is_noreturn = modifiers.contains(TypeModifiers::NORETURN);

        // The definition is compiled under its own type's convention.
        self.current_calling_conv = func.calling_conv;

        let mut ir_func = Function::new(self.emitted_name(func.name), func.return_type);
        ir_func.conv = func.calling_conv;

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
        // A vector is returned as its carrier, which is what the function
        // returns as far as anything below here is concerned; `return`
        // converts to it.
        self.vector_return = self
            .types
            .is_vector(func.return_type)
            .then_some(func.return_type);
        let abi_return = self.abi_return_type(func.return_type, self.current_calling_conv);
        ir_func.return_type = abi_return;
        // Check if function returns a large struct
        // Large structs are returned via a hidden first parameter (sret)
        // that points to caller-allocated space
        let returns_large_struct =
            self.returns_via_hidden_pointer(abi_return, self.current_calling_conv);

        // Argument index offset: if returning large struct, first arg is hidden return pointer
        let arg_offset: u32 = if returns_large_struct { 1 } else { 0 };

        // Add hidden return pointer parameter if needed
        if returns_large_struct {
            let sret_id = self.alloc_pseudo();
            let sret_pseudo = Pseudo::arg(sret_id, 0).with_name("__sret");
            ir_func.add_pseudo(sret_pseudo);
            ir_func.sret = Some(sret_id);
            self.struct_return_ptr = Some(sret_id);
        }

        if self.returns_reg_aggregate(abi_return, self.current_calling_conv) {
            self.reg_aggregate_return_type = Some(abi_return);
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
        // Vector parameters arriving as an aggregate carrier, or by reference.
        let mut vector_memory_params: Vec<VectorMemoryParam> = Vec::new();
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
            //
            // A vector arrives as its carrier.
            let passed_as = match func.param_style {
                _ if self.types.is_vector(param.typ) => {
                    self.abi_type(param.typ, self.current_calling_conv)
                }
                ParamStyle::Prototype => param.typ,
                ParamStyle::IdentifierList => self.types.default_argument_promote(param.typ),
            };
            // A scalar or complex value passed by reference arrives as a
            // pointer, and the `Arg` is recorded as one: the back end types
            // each `Arg` by its parameter, and a `long double` pointer taken
            // for an x87 value is read as one.
            let conv = self.current_calling_conv;
            let by_reference = self.passed_by_reference(passed_as, conv);
            let carried = if by_reference
                && !matches!(
                    self.types.kind(passed_as),
                    TypeKind::Struct | TypeKind::Union
                ) {
                self.types.pointer_to(passed_as)
            } else {
                passed_as
            };
            ir_func.add_param(&name, carried);

            // Create argument pseudo (offset by 1 if there's a hidden return pointer)
            let pseudo_id = self.alloc_pseudo();
            let pseudo = Pseudo::arg(pseudo_id, i as u32 + arg_offset).with_name(&name);
            ir_func.add_pseudo(pseudo);

            // For struct/union types, we'll copy to a local later
            // so member access works properly
            let param_kind = self.types.kind(param.typ);
            if self.types.is_vector(param.typ) {
                // A vector arrives as its carrier: an aggregate's bytes, or a
                // register's bits. The local is the carrier's size and
                // alignment either way, so the carrier's own store fills it.
                if by_reference
                    || matches!(
                        self.types.kind(passed_as),
                        TypeKind::Struct | TypeKind::Union
                    )
                {
                    vector_memory_params.push(VectorMemoryParam {
                        name,
                        symbol: param.symbol,
                        vector: param.typ,
                        carrier: passed_as,
                        arg: pseudo_id,
                    });
                } else {
                    scalar_params.push(ScalarParam {
                        name,
                        symbol: param.symbol,
                        typ: param.typ,
                        passed_as,
                        arg: pseudo_id,
                    });
                }
            } else if by_reference || self.complex_travels_as_bits(param.typ, conv) {
                // Copied out of the caller's copy, or stored from the bits it
                // arrived as: `store_struct_params` knows both.
                struct_params.push((name, param.symbol, param.typ, pseudo_id));
            } else if param_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
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
                let class = get_abi_for_conv(self.current_calling_conv, self.target)
                    .classify_param(param.typ, self.types);
                let is_hfa_param = matches!(
                    class,
                    crate::abi::ArgClass::Hfa { count, .. } if count >= 2 || size > 64
                );
                let is_two_fp_regs =
                    is_hfa_param || size > 64 && size <= 128 && class.is_register_aggregate();
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
                let abi = get_abi_for_conv(conv, self.target);
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

        self.store_vector_memory_params(vector_memory_params);

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
        // `Function::remove_unreachable_blocks`. Before SSA, which then never
        // sees a definition or a phi source in a block that cannot run.
        if let Some(ref mut ir_func) = self.current_func {
            ir_func.remove_unreachable_blocks();
        }

        if let Some(ref mut ir_func) = self.current_func {
            // Every id the linearizer handed out is below its counter; from
            // here on the function allocates its own.
            ir_func.next_pseudo = self.next_pseudo;
            if self.run_ssa {
                ssa_convert(ir_func, self.types);
                // Drop func.locals entries whose Sym is now unused, so the
                // backend regalloc allocates no stack slot for them.
                mem2reg(ir_func);
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
        if self.types.is_scalar(ret_type) && !self.types.is_complex(ret_type) {
            // A scalar the convention returns through the pointer -- a Win64
            // `long double` -- is converted as `return` converts any value
            // (C17 6.8.6.4p3) and stored there.
            let val = self.linearize_converted(e, ret_type);
            let size = self.types.size_bits(ret_type);
            self.emit(Instruction::store(val, sret_ptr, 0, ret_type, size));
            self.emit_return(Instruction::ret_typed(
                Some(sret_ptr),
                self.types.void_ptr_id,
                64,
            ));
            return;
        }
        let src_addr = if self.types.is_complex(ret_type) {
            self.linearize_converted(e, ret_type)
        } else {
            self.linearize_lvalue(e)
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

        // The value is in the caller's buffer before any cleanup runs, so a
        // cleanup that scrubs the variable returned does not reach it.
        self.emit_return(Instruction::ret_typed(
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
    pub(crate) fn returns_reg_aggregate(&self, typ: TypeId, conv: CallingConv) -> bool {
        matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union)
            && self.types.size_bits(typ) > 64
            && !self.returns_via_hidden_pointer(typ, conv)
    }

    /// Whether an argument of type `typ` travels, under `conv`, as a pointer
    /// to a copy the caller made: an AAPCS64 composite over sixteen bytes,
    /// and under Win64 every value that is not 1, 2, 4 or 8 bytes wide. The
    /// one statement of it for both ends of a call -- the caller makes the
    /// copy, the callee receives the pointer.
    pub(crate) fn passed_by_reference(&self, typ: TypeId, conv: CallingConv) -> bool {
        let abi = get_abi_for_conv(conv, self.target);
        abi.indirect_param_is_reference()
            && matches!(
                abi.classify_param(typ, self.types),
                crate::abi::ArgClass::Indirect { .. }
            )
    }

    /// Whether a complex value of type `typ` travels, under `conv`, as the
    /// bits of its value in one integer position rather than by address:
    /// Win64 passes and returns a complex number of 1, 2, 4 or 8 bytes the
    /// way it does an aggregate of that size. The value is then read as the
    /// unsigned integer of that width -- [`TypeTable::unsigned_of_size`].
    pub(crate) fn complex_travels_as_bits(&self, typ: TypeId, conv: CallingConv) -> bool {
        conv == CallingConv::Win64
            && self.types.is_complex(typ)
            && self
                .types
                .unsigned_of_size(self.types.size_bytes(typ))
                .is_some()
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
            // The `Ret` reads the value through the address when the function
            // returns, which is after the cleanups have run.
            let src_addr = self.outlive_cleanups(src_addr, ret_type, 0);
            let mut ret_insn = Instruction::ret_typed(Some(src_addr), ret_type, struct_size);
            ret_insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(vec![], ret_class)));
            self.emit_return(ret_insn);
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
        self.emit_return(ret_insn);
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

    /// Whether running `op` where the program would not have can be seen:
    /// it may raise a floating-point exception ([`FpRaise`]) and the program
    /// may look at the flags (`-ftrapping-math`, the default).
    fn opcode_raises_fp(&self, op: Opcode) -> bool {
        self.trapping_math && op.fp_raise() != FpRaise::Never
    }

    /// [`Self::opcode_raises_fp`], for converting a value of type `from` to
    /// type `to` ([`conversion_raises_fp`]).
    fn conversion_raises_fp(&self, from: TypeId, to: TypeId) -> bool {
        self.trapping_math && conversion_raises_fp(self.types, from, to)
    }

    /// [`Self::conversion_raises_fp`], for converting the value of `expr`, of
    /// type `from`, to type `to`: an integer constant converts exactly where
    /// its value is one of the destination's, so `c ? x : 0` with a `float x`
    /// converts its `0` without a trace.
    fn converting_raises_fp(&self, expr: &Expr, from: TypeId, to: TypeId) -> bool {
        if !self.conversion_raises_fp(from, to) {
            return false;
        }
        match self.types.fp_format(to) {
            Some(fmt) if self.types.is_integer(from) => self
                .const_magnitude(expr, from)
                .is_none_or(|m| !fmt.holds_integer_magnitude(m)),
            _ => true,
        }
    }

    /// The magnitude of the integer constant `expr`, of type `typ`, or `None`
    /// if it is not a constant. A constant is evaluated in an `i128`, which
    /// holds an `unsigned __int128` at or above 2^127 as negative: its bits
    /// are the magnitude, where a signed value's is its absolute value.
    fn const_magnitude(&self, expr: &Expr, typ: TypeId) -> Option<u128> {
        let value = self.eval_const_expr(expr)?;
        Some(if self.types.is_unsigned(typ) {
            value as u128
        } else {
            value.unsigned_abs()
        })
    }

    /// Whether computing `left op right`, the binary expression `expr`, can
    /// raise a floating-point exception ([`FpRaise`]) besides what its
    /// operands raise: converting each operand to the type the operation is
    /// performed at, and the operation itself -- the opcode
    /// [`super::linearize_emit::binary_opcode`] makes of it. Complex and
    /// vector operands are asked about their halves and lanes.
    ///
    /// `&&` and `||` compare each operand with zero, quietly, and raise
    /// nothing of their own.
    fn binary_raises_fp(&self, expr: &Expr, op: BinaryOp, left: &Expr, right: &Expr) -> bool {
        if matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr) {
            return false;
        }
        let (left_typ, right_typ) = (self.expr_type(left), self.expr_type(right));
        // The type `linearize_binary` converts both operands to.
        let operand_typ = if op.is_comparison() {
            self.types.common_type(left_typ, right_typ)
        } else {
            self.expr_type(expr)
        };
        // What one lane or half is computed at.
        let scalar = |t: TypeId| match self.types.vector_lanes(t) {
            Some((lane, _)) => lane,
            None => self.types.complex_base(t),
        };
        let operand = scalar(operand_typ);
        let converts = |e: &Expr, t: TypeId| self.converting_raises_fp(e, scalar(t), operand);
        converts(left, left_typ)
            || converts(right, right_typ)
            || self.opcode_raises_fp(super::linearize_emit::binary_opcode(
                self.types, op, operand,
            ))
    }

    /// Whether the computation a library call stands for can raise a
    /// floating-point exception: the answer of the opcode it is computed by
    /// (`compute_library_call`).
    fn library_call_raises_fp(&self, func: InlineLibraryFn) -> bool {
        let op = match func {
            InlineLibraryFn::Sqrt(_) => Opcode::Sqrt,
            InlineLibraryFn::RoundToIntegral(how) => Opcode::RoundToIntegral(how),
            InlineLibraryFn::FMin => Opcode::FMin,
            InlineLibraryFn::FMax => Opcode::FMax,
            InlineLibraryFn::Fma => Opcode::Fma,
            InlineLibraryFn::Fabs => Opcode::Fabs,
            InlineLibraryFn::CopySign => Opcode::CopySign,
            InlineLibraryFn::Conjugate => Opcode::FNeg,
            InlineLibraryFn::IntAbs
            | InlineLibraryFn::ComplexReal
            | InlineLibraryFn::ComplexImag
            | InlineLibraryFn::Memory(_) => return false,
        };
        self.opcode_raises_fp(op)
    }

    /// Whether a conditional's arm `arm` may be evaluated whichever way the
    /// condition goes, converted to the conditional's type `result_typ`:
    /// it is pure, and converting it raises no floating-point exception --
    /// `c ? n : 0.0f` with a large `int n` is inexact where `c` is false.
    pub(crate) fn is_speculatable_arm(&self, arm: &Expr, result_typ: TypeId) -> bool {
        self.is_pure_expr(arm) && !self.converting_raises_fp(arm, self.expr_type(arm), result_typ)
    }

    /// Check if an expression is "pure": safe to evaluate where the program
    /// would not have. Pure expressions can be speculatively evaluated,
    /// enabling cmov/csel codegen.
    ///
    /// An expression is pure if it contains NO:
    /// - Function calls
    /// - Volatile accesses
    /// - Pre/post increment/decrement (++, --)
    /// - Assignments (=, +=, -=, etc.)
    /// - Statement expressions (GNU extension with potential side effects)
    /// - Operation that can raise a floating-point exception ([`FpRaise`]):
    ///   c17 defines `__STDC_IEC_559__`, so the flags are something the
    ///   program observes, and `c ? a * b : 0` evaluated as a select reports
    ///   an overflow where `c` is false.
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
            // on division by zero, so they're never pure. Floating
            // arithmetic can raise overflow, inexact or invalid, and a
            // floating relational raises invalid for a NaN operand (C17
            // F.9.3): `isnan(x) ? 0 : x < y`.
            ExprKind::Binary {
                op, left, right, ..
            } => {
                !matches!(op, BinaryOp::Div | BinaryOp::Mod)
                    && !self.binary_raises_fp(expr, *op, left, right)
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

            // Ternary is pure if all parts are pure, each arm converted to
            // the conditional's type.
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                let typ = self.expr_type(expr);
                self.is_pure_expr(cond)
                    && self.is_speculatable_arm(then_expr, typ)
                    && self.is_speculatable_arm(else_expr, typ)
            }

            // `a ?: b` evaluates `a` once and `b` only when `a` is false, so
            // it is pure exactly when both are. `a` converted is the value
            // the program takes where it is nonzero, and converting a zero
            // is exact.
            ExprKind::CondElvis { cond, else_expr } => {
                self.is_pure_expr(cond) && self.is_speculatable_arm(else_expr, self.expr_type(expr))
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

            // Casts are pure if the operand is pure and the conversion
            // raises nothing: `(float)d` can overflow, `(int)d` is invalid
            // for a NaN.
            ExprKind::Cast { expr: inner, .. } => {
                self.is_pure_expr(inner)
                    && !self.converting_raises_fp(
                        inner,
                        self.expr_type(inner),
                        self.expr_type(expr),
                    )
            }

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
                    && !self.library_call_raises_fp(*func)
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
            | ExprKind::BuiltinComplex { .. }
            | ExprKind::VectorShuffle { .. }
            | ExprKind::ConvertVector { .. } => false,
        }
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
    /// threshold [`Self::read_object`] applies. A complex value always
    /// travels by address, and is not asked about here.
    ///
    /// A zero-sized one -- GNU's `struct {}`, or `struct { int a[0]; }` --
    /// does not: it has no bits to carry, and loading "nothing" from it was
    /// lowered as a one-byte access that stored over its neighbour.
    pub(crate) fn aggregate_travels_by_value(&self, typ: TypeId) -> bool {
        matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union)
            && (1..=64).contains(&self.types.size_bits(typ))
    }

    /// Whether an object of type `typ`, read as an rvalue, yields its
    /// address rather than its value: an array or a `va_list` that is one
    /// decays (C17 6.3.2.1p3, 7.16p3), a function designator converts to a
    /// pointer (6.3.2.1p4), and an aggregate that does not travel by value
    /// ([`Self::aggregate_travels_by_value`]) is used where it lies.
    pub(crate) fn object_reads_as_address(&self, typ: TypeId) -> bool {
        match self.types.kind(typ) {
            TypeKind::Array | TypeKind::Function => true,
            TypeKind::VaList => !self.types.va_list_is_pointer(),
            TypeKind::Struct | TypeKind::Union => !self.aggregate_travels_by_value(typ),
            _ => false,
        }
    }

    /// Whether an object of type `typ` has no value to read or write, which
    /// is then reported: C17 6.3.2.1p2 leaves lvalue conversion of an
    /// incomplete non-array type undefined, and gcc rejects it. Only reached
    /// through a forward-declared `enum` (a GNU extension), or a structure or
    /// union read where the parser let it through: every other value use of
    /// an incomplete type is a constraint the parser enforces. Without this
    /// the zero-width value reached a conversion that cannot be emitted.
    pub(crate) fn reject_incomplete_object(&self, typ: TypeId) -> bool {
        let incomplete = matches!(
            self.types.kind(typ),
            TypeKind::Enum | TypeKind::Struct | TypeKind::Union
        ) && !self.types.is_composite_complete(typ);
        if incomplete {
            let named = self.types.format_type(typ, Some(self.strings));
            let pos = self.current_pos.unwrap_or_default();
            crate::diag::error_args(pos, "invalid use of incomplete type '{0}'", &[&named]);
        }
        incomplete
    }

    /// Read the object of type `typ` at `place` as an rvalue: its address
    /// when [`Self::object_reads_as_address`] says so, else its value. Every
    /// expression that designates an object -- a name, `*p`, `s.m`, `a[i]`,
    /// a compound literal -- reads it here, so they cannot disagree about
    /// which objects travel by address.
    pub(crate) fn read_object(&mut self, place: ObjectPlace, typ: TypeId) -> PseudoId {
        if self.reject_incomplete_object(typ) {
            return self.emit_const(0, self.types.int_id);
        }
        if self.object_reads_as_address(typ) {
            return match place {
                ObjectPlace::Sym(sym) => {
                    // An array's address is its first element's (6.3.2.1p3).
                    let pointee = match self.types.kind(typ) {
                        TypeKind::Array => self.types.base_type(typ).unwrap_or(self.types.int_id),
                        _ => typ,
                    };
                    let result = self.alloc_reg_pseudo();
                    let ptr_type = self.types.pointer_to(pointee);
                    self.emit(Instruction::sym_addr(result, sym, ptr_type));
                    result
                }
                ObjectPlace::At(base, offset) => self.offset_address(base, offset),
            };
        }
        let (base, offset) = match place {
            ObjectPlace::Sym(sym) => (sym, 0),
            ObjectPlace::At(base, offset) => (base, offset),
        };
        let result = self.alloc_reg_pseudo();
        let size = self.types.size_bits(typ);
        self.emit(Instruction::load(result, base, offset, typ, size));
        result
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
                let local = self.locals.get(symbol_id).cloned();
                let (sym, typ) = match local {
                    Some(LocalVarInfo {
                        binding: LocalBinding::Frame { sym, storage },
                        typ,
                        ..
                    }) => {
                        // When the slot holds a pointer to the object -- a
                        // VLA's `alloca`d storage, or a `va_list` parameter
                        // -- the object's address is that pointer's value,
                        // not the slot's. Taking the slot's address made `&a`
                        // differ from `a` for every VLA, so `int (*p)[n] =
                        // &a` pointed at the pointer and read back garbage.
                        if let Storage::Indirect(ptr_type) = storage {
                            let result = self.alloc_pseudo();
                            let size = self.types.size_bits(ptr_type);
                            self.emit(Instruction::load(result, sym, 0, ptr_type, size));
                            return result;
                        }
                        (sym, typ)
                    }
                    Some(LocalVarInfo {
                        binding: LocalBinding::Static { global },
                        typ,
                        ..
                    }) => (self.sym_pseudo(global), typ),
                    Some(LocalVarInfo {
                        binding: LocalBinding::ExtentsOnly,
                        ..
                    }) => unreachable!("no identifier names a type name's extents"),
                    None => {
                        let name = self.symbol_name(*symbol_id);
                        (self.sym_pseudo(name), self.expr_type(expr))
                    }
                };
                let result = self.alloc_pseudo();
                self.emit(Instruction::sym_addr(
                    result,
                    sym,
                    self.types.pointer_to(typ),
                ));
                result
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
                let member_info = self
                    .types
                    .find_member(struct_type, *member)
                    .unwrap_or_else(|| MemberInfo::standing_in(self.expr_type(expr)));

                self.offset_address(base, member_info.offset as i64)
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
                let member_info = self
                    .types
                    .find_member(struct_type, *member)
                    .unwrap_or_else(|| MemberInfo::standing_in(self.expr_type(expr)));

                self.offset_address(ptr, member_info.offset as i64)
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
                let sym_id = self.materialize_compound_literal(*typ, elements);
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
    pub(crate) fn linearize_cast(&mut self, inner_expr: &Expr, cast_type: TypeId) -> PseudoId {
        // The operand's value, not the designator: a function or an array is
        // converted from the pointer it decays to.
        let src_type = self.types.decayed_value(self.expr_type(inner_expr));

        // C17 6.3.2.2: a cast to `void` evaluates the operand and discards
        // the value, converting nothing. This has to come before the complex
        // arm as well as before the arithmetic ones: `void` is neither
        // complex nor floating, so a complex operand took the complex-to-real
        // conversion and a floating one the float-to-integer arm further
        // down, and `(void)z` became a `cvttsd2si` that raises `FE_INVALID`
        // for a NaN. The optimizer deleted the dead conversion at -O1 and
        // above, so only -O0 raised it.
        if self.types.kind(cast_type) == TypeKind::Void {
            return self.linearize_expr(inner_expr);
        }

        // Into or out of a complex type (C17 6.3.1.7), by the rule every
        // converting site shares.
        if self.types.is_complex(src_type) || self.types.is_complex(cast_type) {
            return self.linearize_converted(inner_expr, cast_type);
        }

        // A GNU vector's bits, reinterpreted.
        if self.types.is_vector(src_type) || self.types.is_vector(cast_type) {
            return self.linearize_vector_cast(inner_expr, cast_type);
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
        let struct_type = self.expr_type(inner_expr);
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
        let struct_type = self
            .types
            .base_type(ptr_type)
            .unwrap_or_else(|| self.expr_type(expr));
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
    /// ordinary `int` and DCE deleted the load from `-O1` up. It differs from
    /// the member's type only in qualifiers, so its width, sign and kind are
    /// the member's; the two disagree otherwise only once the parser has
    /// already reported an unknown member. It also stands in for the member
    /// type entirely when the lookup fails here.
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

        if let Some(bf) = member_info.bitfield() {
            return self.emit_bitfield_load(base, bf, access_typ);
        }
        self.read_object(ObjectPlace::At(base, member_info.offset as i64), access_typ)
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
        let elem_type = self
            .types
            .arithmetic_pointee(ptr_typ)
            .unwrap_or(self.types.char_id);
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
            self.named_local(dim_sym_id, dim_var_name, ulong_type, self.current_bb, None);

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
    /// The family exists in C because the ordinary relational operators
    /// raise `FE_INVALID` on an unordered pair and these do not: each is the
    /// *quiet* comparison (`fp_compare_opcode`), where `<` is the signaling
    /// one (`float_comparison`).
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
            FpCompare::Unordered => self.emit_unordered(a, b, typ),
            _ => unreachable!("{cmp:?} is one comparison"),
        }
    }

    /// Whether `a` and `b` are unordered: `isnan(a) || isnan(b)`, spelled as
    /// the self-comparison `linearize_fp_test` uses for `isnan`, which is
    /// quiet.
    pub(crate) fn emit_unordered(&mut self, a: PseudoId, b: PseudoId, typ: TypeId) -> PseudoId {
        let a_nan = self.emit_compare(Opcode::FCmpONe, a, a, typ);
        let b_nan = self.emit_compare(Opcode::FCmpONe, b, b, typ);
        self.emit_bool_combine(Opcode::Or, a_nan, b_nan)
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

        self.read_object(ObjectPlace::At(addr, 0), elem_type)
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

    /// The address of a fresh copy of an argument passed by reference, which
    /// the callee owns and may write.
    ///
    /// `val` is the argument's address when `by_address`, its value
    /// otherwise. Passing the original's address instead was invisible
    /// c17-to-c17 -- a c17 callee copies out of it first -- but a gcc callee
    /// that assigned to its parameter wrote through into the caller's object.
    pub(crate) fn argument_copy(
        &mut self,
        val: PseudoId,
        typ: TypeId,
        by_address: bool,
        vol: BlockVolatility,
    ) -> PseudoId {
        let copy = self.frame_temp("__argcopy", typ);
        let copy_addr = self.alloc_reg_pseudo();
        self.emit(Instruction::sym_addr(
            copy_addr,
            copy,
            self.types.pointer_to(typ),
        ));
        if by_address {
            let bytes = self.types.size_bytes(typ) as i64;
            self.emit_block_copy(copy_addr, val, bytes, vol);
        } else {
            let size = self.types.size_bits(typ);
            self.emit(Instruction::store(val, copy, 0, typ, size));
        }
        copy_addr
    }

    /// An argument of type `passed`, as convention `conv` hands it over.
    ///
    /// An aggregate has already been settled by the argument loop, which
    /// copies one passed by reference as it materializes it. What remains
    /// is the Win64 treatment of a value that is not an aggregate: a scalar
    /// or complex value passed by reference goes as a pointer to a copy --
    /// `val` is a complex value's address, or a scalar's value -- and a
    /// complex value of 1, 2, 4 or 8 bytes goes as the bits it holds.
    fn pass_by_convention(&mut self, val: PseudoId, passed: TypeId, conv: CallingConv) -> PseudoId {
        if matches!(self.types.kind(passed), TypeKind::Struct | TypeKind::Union) {
            return val;
        }
        if self.passed_by_reference(passed, conv) {
            let by_address = self.types.is_complex(passed);
            return self.argument_copy(val, passed, by_address, BlockVolatility::default());
        }
        if self.complex_travels_as_bits(passed, conv) {
            let bytes = self.types.size_bytes(passed);
            let bits = self
                .types
                .unsigned_of_size(bytes)
                .expect("checked by complex_travels_as_bits");
            let loaded = self.alloc_reg_pseudo();
            let size = self.types.size_bits(bits);
            self.emit(Instruction::load(loaded, val, 0, bits, size));
            return loaded;
        }
        val
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
        let target = self.lower_callee(func_expr);

        let sig = self.callee_signature(func_expr);
        let conv = sig.conv;
        // The function's return type -- as a vector's carrier, which is what
        // the call returns; it is made the vector again below.
        let expr_typ = self.expr_type(expr);
        let typ = self.abi_return_type(expr_typ, conv);
        // A library function is known by its System V signature: what folds
        // or lowers a call to `memcpy` makes a System V call, and a callee
        // declared `ms_abi` is some other function of that name.
        let known = known.filter(|_| conv == CallingConv::C);

        // Check if function returns a large struct or complex type
        // Large structs: allocate space and pass address as hidden first argument
        // Complex types: allocate local storage for result (needs stack for 16-byte value)
        // Two-register structs (9-16 bytes): allocate local storage, codegen stores two regs
        let typ_kind = self.types.kind(typ);
        let returns_large_struct = self.returns_via_hidden_pointer(typ, conv);
        // An aggregate that comes back in registers still needs somewhere to
        // land, and the backend writes the registers into this local. There is
        // no upper size bound: an HFA of four `double`s is thirty-two bytes
        // and still returns in registers, not through the hidden pointer.
        let returns_reg_aggregate = self.returns_reg_aggregate(typ, conv);
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
        } else if matches!(typ_kind, TypeKind::Struct | TypeKind::Union) {
            // Every other struct/union return -- one register's worth, or a
            // zero-sized one, which comes back in nothing: allocate local
            // storage so the result has a stable address. The codegen
            // stores RAX (or XMM0) to this location. Without this, the
            // result pseudo holds a raw value which emit_assign's block_copy
            // would incorrectly dereference as a pointer.
            let local_sym = self.frame_temp("__sret1", typ);
            (local_sym, Vec::new(), Vec::new())
        } else {
            let result = self.alloc_pseudo();
            (result, Vec::new(), Vec::new())
        };

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
            let (val, passed) = self.lower_call_arg(a, arg_idx, &sig);
            arg_vals.push(val);
            arg_types_vec.push(passed);
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

        let abi = get_abi_for_conv(conv, self.target);
        let param_classes: Vec<_> = arg_types_vec
            .iter()
            .map(|&t| abi.classify_param(t, self.types))
            .collect();
        let ret_class = abi.classify_return(typ, self.types);
        let facts = CallFacts {
            variadic_arg_start: sig.variadic_arg_start,
            ends_with_va_arg_pack,
            is_noreturn: sig.is_noreturn,
            binding,
            known,
            abi_info: Box::new(CallAbiInfo::with_conv(param_classes, ret_class, conv)),
        };

        if returns_large_struct {
            // For large struct returns, the call's value is the address of
            // the local the callee wrote, and the pointer is 64 bits wide.
            let result = self.alloc_reg_pseudo();
            let ptr_typ = self.types.pointer_to(typ);
            let call_args = CallArgs {
                vals: arg_vals,
                types: arg_types_vec,
            };
            self.build_call(target, call_args, result, (ptr_typ, 64), facts);
            // A scalar returned through the pointer is read back as the
            // call's value; anything else is used by address, as stored.
            if self.types.is_scalar(typ) && !self.types.is_complex(typ) {
                let val = self.alloc_reg_pseudo();
                let size = self.types.size_bits(typ);
                self.emit(Instruction::load(val, result_sym, 0, typ, size));
                return val;
            }
        } else {
            let ret_size = self.types.size_bits(typ);
            let call_args = CallArgs {
                vals: arg_vals,
                types: arg_types_vec,
            };
            self.build_call(target, call_args, result_sym, (typ, ret_size), facts);
        }
        if self.types.is_vector(expr_typ) {
            return self.vector_returned(result_sym, typ, expr_typ, conv);
        }
        result_sym
    }

    /// What a call calls: the named function, or the function pointer the
    /// callee expression evaluates to. Emits that evaluation, so it runs
    /// before the arguments are lowered.
    fn lower_callee(&mut self, func_expr: &Expr) -> CallTarget {
        // Determine if this is a direct or indirect call.
        // We need to check the TYPE of the function expression:
        // - If it's TypeKind::Function, it's a direct call to a function
        // - If it's TypeKind::Pointer to Function, it's an indirect call through function pointer
        let is_function_pointer = func_expr.typ.is_some_and(|t| {
            let typ = self.types.get(t);
            typ.kind == TypeKind::Pointer
        });

        match &func_expr.kind {
            ExprKind::Ident(symbol_id) if !is_function_pointer => {
                // Direct call to named function (not a function pointer variable)
                CallTarget::Direct(self.symbol_name(*symbol_id))
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
                        CallTarget::Indirect(self.linearize_expr(operand))
                    } else {
                        // Operand is pointer-to-pointer - dereference to get function pointer
                        CallTarget::Indirect(self.linearize_expr(func_expr))
                    }
                } else {
                    // Unknown case - try to linearize the full expression
                    CallTarget::Indirect(self.linearize_expr(func_expr))
                }
            }
            _ => {
                // Indirect call through function pointer variable: fp(args)
                // This includes identifiers that are function pointer variables
                CallTarget::Indirect(self.linearize_expr(func_expr))
            }
        }
    }

    /// What the callee expression's type says about the call: see
    /// [`CalleeSignature`].
    fn callee_signature(&self, func_expr: &Expr) -> CalleeSignature {
        // The callee's convention classifies everything below: its return,
        // which arguments it takes by reference, and the registers.
        let conv = func_expr
            .typ
            .map_or(CallingConv::C, |t| CallingConv::of_callee(t, self.types));

        // C17 6.5.2.2p1 lets the function designator be a function *or* a
        // pointer to one, and everything the call needs to know lives on the
        // function type either way. Reading it off the pointer found nothing:
        // a call through a pointer converted no argument to its parameter,
        //
        //   void f(double); void (*p)(double) = f; p(1);
        //
        // passing the integer 1 where the callee read a `double`, and a call
        // through a pointer to a variadic function was no variadic call, so
        // its trailing arguments took no default argument promotions.
        let callee = func_expr
            .typ
            .and_then(|t| self.types.callee_function_type(t))
            .map(|f| self.types.get(f));
        // The parameter types, for the conversions to them: transparent for
        // a call the ABI carries out, but an inlined callee reads the
        // argument pseudo as it is.
        let params = callee.and_then(|ft| ft.params.clone());
        // A call through a function type with no prototype: C17 6.5.2.2p6
        // gives every argument the default argument promotions, as it does
        // a variadic one, and an identifier-list definition receives them so
        // (see `ParamStyle`). Passing a `float` as a float had a gcc-compiled
        // K&R callee read a double out of a register that held a single, and
        // a `char` took Apple arm64's one-byte stack slot where the callee
        // reads an `int`.
        let unprototyped = callee.is_some_and(|ft| ft.params.is_none());
        // The variadic arguments start after the fixed parameters.
        let variadic_arg_start = callee
            .filter(|ft| ft.variadic)
            .and_then(|ft| ft.params.as_ref().map(|p| p.len()));
        let is_noreturn = callee.is_some_and(|ft| ft.noreturn);

        CalleeSignature {
            conv,
            params,
            unprototyped,
            variadic_arg_start,
            is_noreturn,
        }
    }

    /// Lower the call argument `a`, the `arg_idx`-th, to the value the call
    /// passes and the type the ABI classifies it by.
    ///
    /// For large structs, pass by reference (address) instead of by value.
    /// For complex types, pass address so codegen can load real/imag into XMM
    /// registers. For arrays (including VLAs), decay to pointer.
    fn lower_call_arg(
        &mut self,
        a: &Expr,
        arg_idx: usize,
        sig: &CalleeSignature,
    ) -> (PseudoId, TypeId) {
        let conv = sig.conv;
        let arg_type = self.expr_type(a);
        let arg_kind = self.types.kind(arg_type);
        let param = sig
            .params
            .as_ref()
            .and_then(|params| params.get(arg_idx).copied());
        let (arg_val, passed) = if self.types.is_vector(arg_type) {
            // As its carrier, which `pass_by_convention` then treats as any
            // value of that type.
            self.lower_vector_arg(a, conv)
        } else if (arg_kind == TypeKind::Struct || arg_kind == TypeKind::Union)
            && (self.types.size_bits(arg_type) > 64
                || self.passed_by_reference(arg_type, conv)
                || self.passed_in_memory(arg_type, conv))
        {
            self.lower_struct_arg(a, arg_type, conv)
        } else if let Some(pt) =
            param.filter(|pt| self.types.is_complex(arg_type) || self.types.is_complex(*pt))
        {
            // A complex argument or a complex parameter: C17 6.5.2.2p7
            // converts the argument as if by assignment, and a complex value
            // -- here or in the callee -- travels by address, which the
            // backend loads into registers. Converted to the parameter's
            // precision too: a complex value is read with its base type's
            // stride, so a `float _Complex` handed unconverted to a
            // `double _Complex` parameter arrived as `2+1i` for `1+2i`. The
            // type recorded for the ABI moves with it, or the classification
            // is made for a width that is no longer there.
            (self.linearize_converted(a, pt), pt)
        } else if self.types.is_complex(arg_type) {
            // No prototype, or a variadic argument: nothing says what
            // precision the callee wants, so it travels as written.
            (self.complex_operand_addr(a), arg_type)
        } else if arg_kind == TypeKind::Array {
            // Array decay to pointer (C99 6.3.2.1)
            // This applies to both fixed-size arrays and VLAs
            self.lower_decaying_arg(a, arg_type, param)
        } else if arg_kind == TypeKind::VaList && !self.types.va_list_is_pointer() {
            // va_list decay to pointer (C99 7.15.1)
            // va_list is defined as __va_list_tag[1] (an array), so it decays to
            // a pointer when passed to a function taking va_list parameter.
            // Where va_list is already a pointer there is nothing to decay, and
            // the ordinary scalar path below passes it by value.
            let passed = self.types.pointer_to(arg_type);
            (self.linearize_lvalue(a), passed)
        } else if arg_kind == TypeKind::Function {
            // Function decay to pointer (C99 6.3.2.1)
            // Function names passed as arguments decay to function pointers
            self.lower_decaying_arg(a, arg_type, param)
        } else {
            self.lower_scalar_arg(a, arg_idx, arg_type, param, sig)
        };
        (self.pass_by_convention(arg_val, passed, conv), passed)
    }

    /// An array or function-designator argument: the pointer it decays to
    /// (C17 6.3.2.1p3-4), converted to its parameter's type as if by
    /// assignment when a prototype gives one (6.5.2.2p7).
    ///
    /// The conversion is not always the identity: a `_Bool` parameter
    /// receives whether the address is null. Passing the decayed pointer as
    /// it stood handed `take_bool(h)` the raw address, which the callee
    /// read through its low byte -- zero for a function or array aligned to
    /// 256, so `take_bool(arr)` was false.
    fn lower_decaying_arg(
        &mut self,
        a: &Expr,
        arg_type: TypeId,
        param: Option<TypeId>,
    ) -> (PseudoId, TypeId) {
        match param {
            Some(pt) => (self.linearize_converted(a, pt), pt),
            None => (self.linearize_expr(a), self.types.decayed_value(arg_type)),
        }
    }

    /// Whether the convention passes an argument of type `typ` in memory: an
    /// eight-byte struct holding a `long double`, or a one-lane floating
    /// vector, is MEMORY class on System V however small. Taken for a value
    /// in a register, its bytes were handed over as its address.
    fn passed_in_memory(&self, typ: TypeId, conv: CallingConv) -> bool {
        matches!(
            get_abi_for_conv(conv, self.target).classify_param(typ, self.types),
            crate::abi::ArgClass::Indirect { .. }
        )
    }

    /// A struct or union argument wider than a register, or one the
    /// convention passes by reference: its address, and the type the ABI
    /// classifies it by.
    fn lower_struct_arg(
        &mut self,
        a: &Expr,
        arg_type: TypeId,
        conv: CallingConv,
    ) -> (PseudoId, TypeId) {
        let size_bits = self.types.size_bits(arg_type);
        let abi = get_abi_for_conv(conv, self.target);
        let class = abi.classify_param(arg_type, self.types);
        let passed = if size_bits > 128 {
            // Large struct (> 16 bytes): keep struct type so ABI classifies as
            // Indirect/MEMORY. The pseudo is the struct's address: System V
            // copies its bytes to the stack there, and AAPCS64 is handed a
            // copy of it below.
            arg_type
        } else if class.is_register_aggregate()
            // MEMORY class means the bytes go on the stack by value,
            // exactly as an over-sixteen-byte struct already does.
            // Reachable at this size only when an eightbyte holds a
            // `long double`, directly or merged with something else;
            // passing a pointer instead disagreed with gcc silently.
            || matches!(class, crate::abi::ArgClass::Indirect { .. })
        {
            // Medium struct (9-16 bytes) that travels in registers or in
            // memory: keep the struct type, as the ABI decides from it. The
            // pseudo still carries the address; it is the backend that loads
            // the registers out of it.
            arg_type
        } else {
            // Integer or mixed struct: pass as pointer (existing behavior)
            self.types.pointer_to(arg_type)
        };
        // A struct argument travels by address. `linearize_lvalue`
        // materializes an rvalue -- a call returning a struct -- and
        // hands back the temporary's address, so both cases are the
        // same call.
        let addr = self.linearize_lvalue(a);
        let val = if self.passed_by_reference(arg_type, conv) {
            let vol = self.block_volatility(arg_type, self.expr_type(a));
            self.argument_copy(addr, arg_type, true, vol)
        } else {
            addr
        };
        (val, passed)
    }

    /// A scalar argument, converted to its parameter's type `param` when
    /// there is one, and given the default argument promotions where no
    /// prototype covers it.
    fn lower_scalar_arg(
        &mut self,
        a: &Expr,
        arg_idx: usize,
        mut arg_type: TypeId,
        param: Option<TypeId>,
        sig: &CalleeSignature,
    ) -> (PseudoId, TypeId) {
        let mut val = self.linearize_expr(a);

        // Implicit argument conversion when actual type differs from
        // formal parameter type. Covers:
        // - Integer widening: int→long (sign/zero extend)
        // - FP widening/narrowing: float↔double↔long double
        // - Int→FP: uint32_t→double (e.g., log10(uint32_t_val))
        // - FP→Int: rare but legal
        if let Some(param_type) = param {
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

        // C99 6.5.2.2p7: default argument promotions for variadic args,
        // and 6.5.2.2p6 for every argument of an unprototyped call.
        //
        // Both halves have to happen here. The formal-parameter
        // conversion above only runs for an argument a parameter covers,
        // and a variadic argument is by definition past them, so it never
        // runs for these. The cast itself emits no IR either, because
        // emit_convert short-circuits same-size integer conversions --
        // without an explicit promotion the pseudo still holds the
        // sign-extended load, and `printf("%02x", (unsigned char)c)`
        // prints ffffff80 for a negative `signed char`.
        if sig.unprototyped || sig.variadic_arg_start.is_some_and(|v| arg_idx >= v) {
            let promoted = self.types.default_argument_promote(arg_type);
            if promoted != arg_type {
                val = self.emit_convert(val, arg_type, promoted);
                arg_type = promoted;
            }
        }

        (val, arg_type)
    }

    /// Emit the call instruction itself, and the `Unreachable` after a call
    /// that does not return. `result` receives the value of type and width
    /// `ret`.
    fn build_call(
        &mut self,
        target: CallTarget,
        args: CallArgs,
        result: PseudoId,
        ret: (TypeId, u32),
        facts: CallFacts,
    ) {
        let (ret_typ, ret_size) = ret;
        let mut call_insn = match target {
            CallTarget::Indirect(func_addr) => Instruction::call_indirect(
                Some(result),
                func_addr,
                args.vals,
                args.types,
                ret_typ,
                ret_size,
            ),
            CallTarget::Direct(func_name) => Instruction::call(
                Some(result),
                &func_name,
                args.vals,
                args.types,
                ret_typ,
                ret_size,
            ),
        };
        let extra = call_insn.extra_mut();
        extra.variadic_arg_start = facts.variadic_arg_start;
        extra.ends_with_va_arg_pack = facts.ends_with_va_arg_pack;
        extra.is_noreturn_call = facts.is_noreturn;
        extra.callee_binding = facts.binding;
        extra.known = facts.known;
        extra.abi_info = Some(facts.abi_info);
        self.emit(call_insn);
        if facts.is_noreturn {
            self.emit_no_return(
                Instruction::new(Opcode::Unreachable).with_type(self.types.void_id),
            );
        }
    }

    /// Linearize a post-increment or post-decrement expression
    pub(crate) fn linearize_postop(&mut self, operand: &Expr, is_inc: bool) -> PseudoId {
        if self.types.is_vector(self.expr_type(operand)) {
            return self.emit_vector_step(operand, is_inc, false);
        }
        // `x++` on an atomic object is one read-modify-write, and its value is
        // the value *before* the operation -- exactly what fetch-add returns.
        if let Some(result) = self.try_emit_atomic_incdec(operand, is_inc, false) {
            return result;
        }

        // `E++` evaluates `E` exactly once (C17 6.5.2.4p2), so the address is
        // resolved here and serves both the read and the store-back. Reading
        // from the expression and then re-deriving the address for the store
        // ran every subexpression twice: `b[i++]++` incremented `i` twice and
        // updated the wrong element.
        let place = self.resolve_rmw_place(operand);
        let typ = self.expr_type(operand);
        let val = self.load_rmw_place(&place, typ);
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

        // Through the address the read came from. The postfix forms hand back
        // the value from *before* the update, which is already narrowed for a
        // bit-field because `emit_bitfield_load` produced it -- so the store's
        // answer is not needed here.
        self.store_rmw_place(&place, final_result, typ);
        old_val
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

        if self.types.is_vector(left_typ) || self.types.is_vector(right_typ) {
            return self.linearize_vector_binary(op, left, right, result_typ);
        }

        // Check for pointer arithmetic: ptr +/- int or int + ptr. An array or
        // a function designator is a pointer here, as it decays to one.
        let left_is_ptr_or_arr = self.types.arithmetic_pointee(left_typ).is_some();
        let right_is_ptr_or_arr = self.types.arithmetic_pointee(right_typ).is_some();
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
            let left_addr = self.linearize_converted(left, common);
            let right_addr = self.linearize_converted(right, common);
            self.emit_complex_equality(op, left_addr, right_addr, common)
        } else if self.types.is_complex(result_typ) {
            // Complex arithmetic: expand to real/imaginary operations
            // For complex types, we need addresses to load real/imag parts
            // If an operand is not complex (e.g., real scalar), promote it
            let left_addr = self.linearize_converted(left, result_typ);
            let right_addr = self.linearize_converted(right, result_typ);
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
        let (op, dest, second) = match mem {
            MemoryFn::Copy | MemoryFn::CopyToEnd => (BlockOp::Copy, a, b),
            MemoryFn::Set => (BlockOp::Set, a, b),
            MemoryFn::Move => (BlockOp::Move, a, b),
            MemoryFn::MoveSourceFirst => (BlockOp::Move, b, a),
        };
        let ptr = self.types.void_ptr_id;
        let ptr_bits = self.types.size_bits(ptr);
        let result = self.alloc_pseudo();
        let callee = self.library_function_name(op.c_name());
        self.emit(
            Instruction::new(op.opcode())
                .with_func(callee)
                .with_target(result)
                .with_src3(dest, second, n)
                .with_type_and_size(ptr, ptr_bits),
        );
        match mem {
            MemoryFn::CopyToEnd => {
                let end = self.alloc_pseudo();
                self.emit(Instruction::binop(Opcode::Add, end, dest, n, ptr, ptr_bits));
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

        // A vector is operated on lane by lane.
        if self.types.is_vector(self.expr_type(operand)) {
            return match op {
                UnaryOp::PreInc | UnaryOp::PreDec => {
                    self.emit_vector_step(operand, op == UnaryOp::PreInc, true)
                }
                _ => self.linearize_vector_unary(op, operand),
            };
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
            // `j` twice and updated the wrong element.
            let place = self.resolve_rmw_place(operand);
            let typ = self.expr_type(operand);
            let val = self.load_rmw_place(&place, typ);
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

            // Store back through the address the read came from. `++x.f` is
            // `x.f += 1`, whose value is what the field now holds (C17
            // 6.5.16.1p2) -- so `signed int f : 3` at 3 gives -4, not 4. The
            // postfix forms need no such care: they hand back the value
            // loaded before the update, which `emit_bitfield_load` already
            // narrowed.
            return match self.store_rmw_place(&place, final_result, typ) {
                Some((bit_width, field_typ)) => {
                    self.narrow_to_bitfield(final_result, bit_width, field_typ)
                }
                None => final_result,
            };
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

        // The name as written, not an asm label: `int f(void) __asm__("g")`
        // has `__func__` "f", as in gcc.
        let label = self
            .module
            .add_string(self.str(self.current_func_ident).to_string());

        // Create symbol pseudo for the string label
        let sym_id = self.sym_pseudo(label);

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
            let place = match local.binding {
                // A `va_list` parameter where `va_list` is an array: the
                // caller's argument decayed, so the slot holds a pointer to
                // the caller's object (C17 7.16p3).
                LocalBinding::Frame {
                    sym,
                    storage: Storage::Indirect(ptr_type),
                } if self.types.kind(local.typ) == TypeKind::VaList
                    && !self.types.va_list_is_pointer() =>
                {
                    let ptr = self.alloc_reg_pseudo();
                    let ptr_size = self.types.size_bits(ptr_type);
                    self.emit(Instruction::load(ptr, sym, 0, ptr_type, ptr_size));
                    ObjectPlace::At(ptr, 0)
                }
                LocalBinding::Frame { sym, .. } => ObjectPlace::Sym(sym),
                LocalBinding::Static { global } => ObjectPlace::Sym(self.sym_pseudo(global)),
                LocalBinding::ExtentsOnly => {
                    unreachable!("no identifier names a type name's extents")
                }
            };
            self.read_object(place, local.typ)
        }
        // Global variable - create symbol reference and load
        else {
            self.check_inline_static_reference(symbol_id);
            let name_str = self.symbol_name(symbol_id);
            let sym_id = self.sym_pseudo(name_str);
            let typ = self.expr_type(expr);
            self.read_object(ObjectPlace::Sym(sym_id), typ)
        }
    }

    /// The file-scope static that a reference to `symbol_id` in the current
    /// function would break C99 6.7.4p3 by naming, if any: a non-static
    /// inline definition cannot refer to one.
    ///
    /// Decided by what the identifier resolves to. A parameter or block-scope
    /// object spelled like the static has a local binding and is the
    /// function's own object, not the static.
    pub(crate) fn inline_static_reference(&self, symbol_id: SymbolId) -> Option<String> {
        if !self.current_func_is_inline_definition || self.locals.contains_key(&symbol_id) {
            return None;
        }
        let name = self.symbol_name(symbol_id);
        self.file_scope_statics.contains(&name).then_some(name)
    }

    /// Report a reference [`Self::inline_static_reference`] refuses.
    pub(crate) fn check_inline_static_reference(&self, symbol_id: SymbolId) {
        let Some(name) = self.inline_static_reference(symbol_id) else {
            return;
        };
        if let Some(pos) = self.current_pos {
            let msg = format!(
                "inline definition of '{}' cannot reference file-scope static variable '{}'",
                self.current_func_name, name
            );
            // gcc does not enforce this one, so real source contains it --
            // ffmpeg's `dv_guess_qnos` reads a file-scope `static const int`
            // from an inline definition. It is relaxed by `-fpermissive`,
            // which is where c17 keeps the constraints gcc lets through.
            crate::diag::permissive_error(pos, &msg);
        }
    }

    /// Emit a symbol address for a string/wide-string label.
    pub(crate) fn emit_string_sym(&mut self, expr: &Expr, label: String) -> PseudoId {
        let sym_id = self.sym_pseudo(label);
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
            |lin| lin.linearize_converted(then_expr, result_typ),
            |lin| lin.linearize_converted(else_expr, result_typ),
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
        // travels by value and is merged as one, below. A GNU vector always
        // travels by address.
        let aggregate = (matches!(
            self.types.kind(result_typ),
            TypeKind::Struct | TypeKind::Union
        ) && !self.aggregate_travels_by_value(result_typ))
            || self.types.is_vector(result_typ);
        let (merge_typ, size) = if aggregate {
            (self.types.pointer_to(result_typ), self.target.pointer_width)
        } else if self.types.kind(result_typ) == TypeKind::Function {
            (result_typ, 64)
        } else {
            (result_typ, self.types.size_bits(result_typ))
        };

        if self.is_speculatable_arm(then_expr, result_typ)
            && self.is_speculatable_arm(else_expr, result_typ)
            && size <= 64
        {
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
            |lin| lin.linearize_converted(else_expr, result_typ),
        )
    }

    /// The value of a conditional expression whose constant condition
    /// selected `taken`, converted to the conditional's own type.
    fn linearize_constant_arm(&mut self, expr: &Expr, taken: &Expr) -> PseudoId {
        let result_typ = self.expr_type(expr);
        self.linearize_converted(taken, result_typ)
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

        // The left operand converted is the value taken where it is nonzero,
        // and converting a zero is exact, so only the right one is asked.
        if self.is_speculatable_arm(else_expr, result_typ) && size <= 64 {
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

        let op = if width == 32 {
            Opcode::Clz32
        } else {
            Opcode::Clz64
        };
        self.emit_bit_count(op, nonzero, val_typ)
    }

    /// A bit count `op` of `arg`, already converted to the builtin's
    /// unsigned parameter type.
    fn linearize_bit_count(&mut self, op: Opcode, arg: &Expr) -> PseudoId {
        let operand = self.expr_type(arg);
        let val = self.linearize_expr(arg);
        self.emit_bit_count(op, val, operand)
    }

    /// The `int` count `op` of `val`, of type `operand`. The count reads
    /// another type than it produces, so it records the operand
    /// (`Opcode::reads_another_type`).
    fn emit_bit_count(&mut self, op: Opcode, val: PseudoId, operand: TypeId) -> PseudoId {
        let result = self.alloc_pseudo();
        let int = self.types.int_id;
        let mut insn = Instruction::unop(op, result, val, int, self.types.size_bits(int));
        insn.src_typ = Some(operand);
        insn.src_size = self.types.size_bits(operand);
        self.emit(insn);
        result
    }

    pub(crate) fn linearize_compound_literal(&mut self, expr: &Expr) -> PseudoId {
        match &expr.kind {
            ExprKind::CompoundLiteral { typ, elements } => {
                let sym_id = self.materialize_compound_literal(*typ, elements);
                self.read_object(ObjectPlace::Sym(sym_id), *typ)
            }
            _ => unreachable!(),
        }
    }

    /// Create a compound literal's object and initialize it; its `Sym`.
    ///
    /// A compound literal at block scope has automatic storage, so it is an
    /// anonymous frame local. C17 6.7.9p21 zero-initializes every subobject
    /// the list does not name, so an aggregate is zeroed whole first and the
    /// list then writes the members it names.
    fn materialize_compound_literal(&mut self, typ: TypeId, elements: &[InitElement]) -> PseudoId {
        let sym_id = self.alloc_pseudo();
        let name = format!(".compound_literal.{}", sym_id.0);
        self.named_local(sym_id, name, typ, self.current_bb, None);
        if matches!(
            self.types.kind(typ),
            TypeKind::Struct | TypeKind::Union | TypeKind::Array
        ) {
            self.emit_aggregate_zero(sym_id, typ);
        }
        self.linearize_init_list(sym_id, typ, elements);
        sym_id
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

            // A vector is fetched as its carrier, as it was passed.
            ExprKind::VaArg { ap, arg_type } if self.types.is_vector(*arg_type) => {
                // A Microsoft `va_list` walks Win64 arguments.
                let conv = if self.types.is_ms_va_list(self.expr_type(ap)) {
                    CallingConv::Win64
                } else {
                    self.current_calling_conv
                };
                let carrier = self.vector_carrier(*arg_type, conv);
                let fetched = Expr {
                    kind: ExprKind::VaArg {
                        ap: ap.clone(),
                        arg_type: carrier,
                    },
                    ..expr.clone()
                };
                let result = self.linearize_expr(&fetched);
                self.vector_call_result(result, carrier, *arg_type)
            }

            ExprKind::VaArg { ap, arg_type } if self.types.is_ms_va_list(self.expr_type(ap)) => {
                let ap_addr = self.linearize_lvalue(ap);
                self.linearize_ms_va_arg(ap_addr, *arg_type)
            }

            ExprKind::VaCopy { dest, src } if self.types.is_ms_va_list(self.expr_type(dest)) => {
                // A Microsoft `va_list` is a plain pointer: copying it is an
                // assignment.
                let dest_addr = self.linearize_lvalue(dest);
                let val = self.linearize_expr(src);
                let ptr = self.types.ms_va_list_id;
                self.emit(Instruction::store(val, dest_addr, 0, ptr, 64));
                self.alloc_pseudo()
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

    /// `__builtin_va_arg` on a `__builtin_ms_va_list` at `ap_addr`.
    ///
    /// The list is a pointer walking the Microsoft x64 argument positions,
    /// eight bytes each -- the four the callee spilled to its shadow area and
    /// the stacked ones after them -- so this is ordinary IR, with nothing for
    /// a back end to know: step the pointer, follow it once more for a type
    /// passed by reference, and read the value. An aggregate or complex value
    /// lands in a local of its own, as a System V `va_arg` puts it.
    ///
    /// gcc reads a by-reference type -- a three-byte struct, a `long double`
    /// -- out of the position itself, as if it had been passed by value;
    /// every caller, gcc's included, passed a pointer there.
    fn linearize_ms_va_arg(&mut self, ap_addr: PseudoId, typ: TypeId) -> PseudoId {
        let ptr = self.types.ms_va_list_id;
        let slot = self.alloc_reg_pseudo();
        self.emit(Instruction::load(slot, ap_addr, 0, ptr, 64));
        let step = self.emit_const(crate::abi::WIN64_POSITION_BYTES as i128, self.types.long_id);
        let next = self.alloc_reg_pseudo();
        self.emit(Instruction::binop(
            Opcode::Add,
            next,
            slot,
            step,
            self.types.long_id,
            64,
        ));
        self.emit(Instruction::store(next, ap_addr, 0, ptr, 64));
        let at = if self.passed_by_reference(typ, CallingConv::Win64) {
            let target = self.alloc_reg_pseudo();
            self.emit(Instruction::load(target, slot, 0, ptr, 64));
            target
        } else {
            slot
        };
        if self.types.is_aggregate_or_complex(typ) {
            let local = self.frame_temp("__vaarg", typ);
            let bytes = self.types.size_bytes(typ) as i64;
            self.emit_block_copy(local, at, bytes, BlockVolatility::default());
            return local;
        }
        let val = self.alloc_reg_pseudo();
        let size = self.types.size_bits(typ);
        self.emit(Instruction::load(val, at, 0, typ, size));
        val
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

            // The counts: an `int` of a 32- or 64-bit unsigned operand.
            ExprKind::Ctz { arg } => self.linearize_bit_count(Opcode::Ctz32, arg),
            ExprKind::Ctzl { arg } | ExprKind::Ctzll { arg } => {
                self.linearize_bit_count(Opcode::Ctz64, arg)
            }
            ExprKind::Clz { arg } => self.linearize_bit_count(Opcode::Clz32, arg),
            ExprKind::Clzl { arg } | ExprKind::Clzll { arg } => {
                self.linearize_bit_count(Opcode::Clz64, arg)
            }
            ExprKind::Clrsb { arg } => self.linearize_clrsb(arg, 32),
            ExprKind::Clrsbl { arg } | ExprKind::Clrsbll { arg } => self.linearize_clrsb(arg, 64),
            ExprKind::Popcount { arg } => self.linearize_bit_count(Opcode::Popcount32, arg),
            ExprKind::Popcountl { arg } | ExprKind::Popcountll { arg } => {
                self.linearize_bit_count(Opcode::Popcount64, arg)
            }

            ExprKind::Alloca { size } => {
                let size_val = self.linearize_expr(size);
                let result = self.alloc_pseudo();

                let insn = Instruction::new(Opcode::Alloca)
                    .with_target(result)
                    .with_src(size_val)
                    .with_type_and_size(self.types.void_ptr_id, self.ptr_bits());
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
        // Evaluated for its side effects, as gcc evaluates it; what it asks
        // for is read from the expression itself.
        let _ = self.linearize_expr(order);
        let memory_order = self.atomic_order(order, OrderedAccess::ReadModifyWrite);

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

        let old = self.emit_atomic_rmw(&lv, &ca, operand, memory_order);
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

        // `__sync_*` is sequentially consistent and has no order argument.
        let ok = self.emit_atomic_cas(addr, exp_addr, des_val, bits, MemoryOrder::SeqCst);

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
            Some(o) => (
                self.linearize_expr(o),
                self.atomic_order(o, OrderedAccess::of(op)),
            ),
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
                fail_order,
            }
            | ExprKind::C11AtomicCompareExchangeWeak {
                ptr,
                expected,
                desired,
                succ_order,
                fail_order,
            } => {
                // Both are implemented as strong: a weak exchange that never
                // fails spuriously is a conforming weak exchange.
                let ptr_val = self.linearize_expr(ptr);
                let expected_ptr = self.linearize_expr(expected);
                let desired_val = self.linearize_expr(desired);
                // Evaluated for their side effects; what they ask for is read
                // from the expressions themselves.
                let _ = self.linearize_expr(succ_order);
                let _ = self.linearize_expr(fail_order);
                let memory_order = self.cas_order(succ_order, fail_order);
                let ptr_type = self.expr_type(ptr);
                let elem_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                let elem_size = self.types.size_bits(elem_type);
                self.emit_atomic_cas(ptr_val, expected_ptr, desired_val, elem_size, memory_order)
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

            ExprKind::C11AtomicThreadFence { order } => self.emit_fence(order, FenceScope::Thread),
            ExprKind::C11AtomicSignalFence { order } => self.emit_fence(order, FenceScope::Signal),
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
        let sym_pseudo = self.sym_pseudo(sym);
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
                let align = self.types.alignof_value(*typ);
                // _Alignof returns size_t
                let result_typ = self.types.ulong_id;
                self.emit_const(align as i128, result_typ)
            }

            ExprKind::AlignofExpr(inner_expr) => {
                let inner_typ = self.expr_type(inner_expr);
                let align = self.types.alignof_value(inner_typ);
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
                // A `case` or `default` label in here cannot belong to a
                // `switch` outside: entering a statement expression by a
                // switch jump is an error, already reported by
                // `check_jumps_into_protected_scopes`, and the label was never
                // collected for that switch. Hiding the enclosing switches
                // keeps the lowering from placing it there anyway; a switch
                // wholly inside the statement expression still pushes its own.
                let enclosing_switches = std::mem::take(&mut self.switch_stack);
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
                let value =
                    self.outlive_cleanups(value, self.expr_type(result), scope.cleanup_entry);
                self.switch_stack = enclosing_switches;
                self.pop_scope(scope);
                value
            }

            ExprKind::BuiltinComplex { real, imag } => {
                self.linearize_builtin_complex(real, imag, expr)
            }

            ExprKind::VectorShuffle {
                first,
                second,
                selector,
            } => {
                let result_typ = self.expr_type(expr);
                self.linearize_vector_shuffle(first, second.as_deref(), selector, result_typ)
            }

            ExprKind::ConvertVector { value } => {
                let result_typ = self.expr_type(expr);
                self.linearize_convert_vector(value, result_typ)
            }
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
pub(crate) mod test_linearize;
#[cfg(test)]
#[path = "test_linearize_asm.rs"]
mod test_linearize_asm;
#[cfg(test)]
#[path = "test_linearize_assign.rs"]
mod test_linearize_assign;
#[cfg(test)]
#[path = "test_linearize_builtin.rs"]
mod test_linearize_builtin;
#[cfg(test)]
#[path = "test_linearize_call.rs"]
mod test_linearize_call;
#[cfg(test)]
#[path = "test_linearize_cfg.rs"]
mod test_linearize_cfg;
#[cfg(test)]
#[path = "test_linearize_cleanup.rs"]
mod test_linearize_cleanup;
#[cfg(test)]
#[path = "test_linearize_expr.rs"]
mod test_linearize_expr;
#[cfg(test)]
#[path = "test_linearize_init.rs"]
mod test_linearize_init;
#[cfg(test)]
#[path = "test_linearize_memory.rs"]
mod test_linearize_memory;

#[cfg(test)]
#[path = "test_linearize_vector.rs"]
mod test_linearize_vector;

#[cfg(test)]
#[path = "test_linearize_win64.rs"]
mod test_linearize_win64;
