//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Intermediate Representation: basic blocks and typed pseudo-registers in
// Single Static Assignment form, where each variable is assigned exactly
// once so that dataflow analysis and the optimization passes stay simple.
//

pub mod asm_operand;
mod build;
pub mod cfg;
pub(crate) mod constfold;
pub mod constglobal;
pub mod copyprop;
pub mod dataflow;
pub mod dce;
pub mod dominate;
pub mod dse;
pub mod effects;
pub mod escape;
pub mod facts;
pub mod ifconv;
pub mod inline;
pub mod instcombine;
pub mod libcall_fold;
pub mod linearize;
mod linearize_atomic;
mod linearize_cleanup;
mod linearize_emit;
mod linearize_init;
mod linearize_label_diff;
mod linearize_stmt;
mod linearize_vector;
pub mod loadfwd;
pub mod lower;
pub mod mach_o_dtors;
pub mod mem2reg;
pub mod memexpand;
pub mod memloc;
pub(crate) mod padding;
pub mod propagate;
pub mod range;
pub mod sccp;
pub mod ssa;
pub(crate) mod strdata;
mod target_clones;
pub mod tls;
pub mod validate;
pub mod vrp;

use crate::abi::{get_abi_for_conv, ArgClass, CallingConv};
use crate::arch::asm_constraints::{AsmAccess, AsmOperandClass};
use crate::diag::Position;
use crate::float::{FloatVal, IntegralRounding};
use crate::target::Target;
use crate::types::{TypeId, TypeTable};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Operands a typical instruction reads; see [`Instruction::uses`].
const DEFAULT_USE_CAPACITY: usize = 4;
const DEFAULT_PARAM_CAPACITY: usize = 8;

// Call ABI Information

/// ABI classification information for a function call.
///
/// This carries the argument and return value classifications from the
/// frontend through the IR to the backend. It replaces the binary
/// boolean flags with richer type information.
#[derive(Debug, Clone)]
pub struct CallAbiInfo {
    /// Per-argument classification (parallel to src for Call instructions)
    pub params: Vec<ArgClass>,
    /// Return value classification
    pub ret: ArgClass,
    /// The callee's calling convention, which made the classifications
    /// above. It also decides what they cannot say: which register each
    /// argument lands in, and the shadow area a Win64 callee is owed.
    pub conv: CallingConv,
}

impl CallAbiInfo {
    /// Classifications made under the target's own convention, which is
    /// what every call the compiler synthesizes -- a runtime library
    /// routine, `memcpy` -- is made with.
    pub fn new(params: Vec<ArgClass>, ret: ArgClass) -> Self {
        Self::with_conv(params, ret, CallingConv::C)
    }

    /// Classifications made under `conv`, the callee's convention.
    pub fn with_conv(params: Vec<ArgClass>, ret: ArgClass, conv: CallingConv) -> Self {
        Self { params, ret, conv }
    }
}

/// Whether an aggregate of `size_bits` returned in class `ret` is handed back
/// by *address*: the `Ret` carries a pointer to the value's storage rather
/// than the value, and whoever consumes the return has to read the bytes out.
///
/// Three classifications answer yes, and they are exactly the three that no
/// pair of general registers can carry:
///
///   * `X87` -- an aggregate that is nothing but a `long double` comes back in
///     st(0), which is loaded from memory because nothing else holds 80 bits.
///   * `Hfa` -- AAPCS64 returns a homogeneous floating-point aggregate in one
///     V register per element, at any size: four `double`s is thirty-two bytes
///     and still comes back in `d0`-`d3`.
///   * one `Sse` -- a single SSE register holding all sixteen bytes, which on
///     x86-64 is an aggregate whose sole content is a `__float128`: SSE+SSEUP
///     is *one* register, and splitting it into RAX/RDX hands a gcc-compiled
///     caller half a value in the wrong place.
///
/// The size bound is part of the rule, not a caller's business: an aggregate
/// that fits in one register comes back *as* a value, so `struct { float a,
/// b; }` is `Direct { classes: [Sse] }` and yet carries its value. Only past
/// 64 bits is an address handed back.
///
/// One function because the answer is asked in three places -- the return
/// emitter, the flag that tells the inliner what it is splicing, and the
/// inliner's own `Ret` lowering -- and two spellings of it had already
/// drifted: the flag omitted the one-SSE case, so a `struct { __float128 a; }`
/// return reported a value-carrying `Ret`, the inliner spliced the body in and
/// phi-ed the callee's local *address* as though it were the aggregate.
pub fn aggregate_ret_is_address(ret: &ArgClass, size_bits: u32) -> bool {
    use crate::abi::RegClass;
    size_bits > 64
        && match ret {
            ArgClass::X87 { .. } | ArgClass::Hfa { .. } => true,
            ArgClass::Direct { classes, .. } => classes.as_slice() == [RegClass::Sse],
            _ => false,
        }
}

// Instruction sites

/// An instruction's place in a function: `(block index, instruction index)`,
/// both positions in the vectors rather than ids.
pub(crate) type Site = (usize, usize);

// Opcodes

/// What a floating comparison does when an operand is a quiet NaN.
///
/// Every comparison raises invalid for a *signaling* NaN; Annex F leaves
/// those unspecified anyway (C17 F.2.1). The difference is the quiet NaN,
/// the one every invalid operation produces: IEEE 754 (5.11) has the
/// relational predicates signal for it and equality not, and C17 F.9.3 binds
/// `<`, `<=`, `>` and `>=` to the signaling ones, `==` and `!=` and the
/// <math.h> comparison macros (7.12.14) to the quiet ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NanCompare {
    /// Raises nothing: `ucomis*` and `fucomip` on x86-64, `fcmp` on
    /// aarch64, `__eqtf2`/`__netf2`/`__unordtf2` in software.
    Quiet,
    /// Raises invalid: `comis*` and `fcomip` on x86-64, `fcmpe` on aarch64,
    /// `__lttf2`/`__letf2`/`__gttf2`/`__getf2` in software.
    Signaling,
}

/// A floating comparison taken apart ([`Opcode::float_cmp`]): the
/// predicate, and for a relational one what a quiet NaN does.
///
/// Equality carries no [`NanCompare`] because it has only one in C17: the
/// quiet one. `Ne` is C's `!=`, true for an unordered pair; every other
/// predicate is false for one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatCmp {
    Eq,
    Ne,
    Lt(NanCompare),
    Le(NanCompare),
    Gt(NanCompare),
    Ge(NanCompare),
}

impl FloatCmp {
    /// What a quiet NaN operand does to this comparison.
    pub fn nan(self) -> NanCompare {
        match self {
            FloatCmp::Eq | FloatCmp::Ne => NanCompare::Quiet,
            FloatCmp::Lt(n) | FloatCmp::Le(n) | FloatCmp::Gt(n) | FloatCmp::Ge(n) => n,
        }
    }

    /// The same relational predicate, raising invalid for a quiet NaN, or
    /// `None` for equality, which C17 has no signaling form of.
    pub fn signaling(self) -> Option<FloatCmp> {
        use NanCompare::Signaling;
        match self {
            FloatCmp::Eq | FloatCmp::Ne => None,
            FloatCmp::Lt(_) => Some(FloatCmp::Lt(Signaling)),
            FloatCmp::Le(_) => Some(FloatCmp::Le(Signaling)),
            FloatCmp::Gt(_) => Some(FloatCmp::Gt(Signaling)),
            FloatCmp::Ge(_) => Some(FloatCmp::Ge(Signaling)),
        }
    }

    /// The same predicate, raising nothing for a quiet NaN.
    ///
    /// The two give the same answer for every operand pair, and the same
    /// exceptions for every *ordered* one; only an unordered pair tells them
    /// apart. So the quiet form may stand in for the signaling one exactly
    /// where the operands are known to be ordered.
    pub fn quiet(self) -> FloatCmp {
        match self {
            FloatCmp::Eq | FloatCmp::Ne => self,
            FloatCmp::Lt(_) => FloatCmp::Lt(NanCompare::Quiet),
            FloatCmp::Le(_) => FloatCmp::Le(NanCompare::Quiet),
            FloatCmp::Gt(_) => FloatCmp::Gt(NanCompare::Quiet),
            FloatCmp::Ge(_) => FloatCmp::Ge(NanCompare::Quiet),
        }
    }
}

impl From<FloatCmp> for Opcode {
    fn from(cmp: FloatCmp) -> Opcode {
        use NanCompare::*;
        match cmp {
            FloatCmp::Eq => Opcode::FCmpOEq,
            FloatCmp::Ne => Opcode::FCmpONe,
            FloatCmp::Lt(Quiet) => Opcode::FCmpOLt,
            FloatCmp::Le(Quiet) => Opcode::FCmpOLe,
            FloatCmp::Gt(Quiet) => Opcode::FCmpOGt,
            FloatCmp::Ge(Quiet) => Opcode::FCmpOGe,
            FloatCmp::Lt(Signaling) => Opcode::FCmpsOLt,
            FloatCmp::Le(Signaling) => Opcode::FCmpsOLe,
            FloatCmp::Gt(Signaling) => Opcode::FCmpsOGt,
            FloatCmp::Ge(Signaling) => Opcode::FCmpsOGe,
        }
    }
}

/// Whether an operation can raise a floating-point exception: set a flag
/// `fetestexcept` reads (C17 7.6.2) -- invalid, divide-by-zero, overflow,
/// underflow or inexact.
///
/// c17 defines `__STDC_IEC_559__`, so those flags are part of what a program
/// observes, and an operation that may raise one must run only where the
/// program runs it: `c ? a * b : 0` evaluated as a select reports an
/// overflow nothing caused. Asked through
/// [`Instruction::may_raise_fp_exception`], by every place that would run an
/// operation on a path that would not have -- `ifconv`, and the linearizer
/// deciding whether a `?:` arm may become a select.
///
/// A *signaling* NaN operand raises invalid in nearly everything; Annex F
/// leaves signaling NaNs unspecified (C17 F.2.1), so `Never` means "for any
/// other operand".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FpRaise {
    /// Raises nothing: every integer and memory operation, and the
    /// floating ones that are exact for every operand and only move or
    /// test bits -- negation, `fabs`, `copysign`, `signbit` (IEEE 754 5.5.1,
    /// C17 F.10.8) -- and the quiet comparisons.
    Never,
    /// May raise: arithmetic, which rounds and can overflow or underflow;
    /// division, which can divide by zero; square root of a negative; a
    /// signaling comparison of a NaN (C17 F.9.3); `fmin`/`fmax`, which x86-64
    /// computes with `minsd`/`maxsd`, raising invalid for a quiet NaN; the
    /// round-to-integral family, of which `rint` raises inexact; a call or an
    /// `asm`, which may do anything.
    May,
    /// A conversion: raises exactly when the destination does not hold every
    /// value of the source. See [`conversion_raises_fp`].
    Conversion,
}

/// Whether converting a value of type `from` to type `to` can raise a
/// floating-point exception ([`FpRaise`]).
///
/// * Floating to floating: not when `to` holds every value of `from`
///   (C17 6.3.1.5p1) -- `float` to `double` is exact, and a quiet NaN
///   stays one. Narrowing can overflow, underflow and be inexact.
/// * Floating to integer: always. A NaN, an infinity or a value out of range
///   raises invalid (C17 F.4), and a fraction may raise inexact.
/// * Integer to floating: not when the format's significand holds every
///   value of the integer type -- `int` to `double` -- and otherwise a large
///   one is inexact (C17 6.3.1.4p2, F.4): `long` to `double`, `int` to
///   `float`.
/// * Anything to `_Bool`: no. A value becomes 0 or 1 by comparing it with
///   zero, quietly (C17 6.3.1.2).
///
/// A complex type converts as its halves do (C17 6.3.1.6-7). Neither type
/// floating -- integers, pointers, a GNU vector, which converts by
/// reinterpreting its bits -- raises nothing, and nor does a cast to `void`.
pub fn conversion_raises_fp(types: &TypeTable, from: TypeId, to: TypeId) -> bool {
    if types.kind(to) == crate::types::TypeKind::Bool {
        return false;
    }
    let (from, to) = (types.complex_base(from), types.complex_base(to));
    match (types.fp_format(from), types.fp_format(to)) {
        (Some(f), Some(t)) => !t.holds(f),
        (Some(_), None) => types.is_integer(to),
        (None, Some(t)) => !t.holds_integer(types.size_bits(from), !types.is_unsigned(from)),
        (None, None) => false,
    }
}

/// IR opcodes for the intermediate representation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    // Function entry
    Entry,

    // Terminators - end a basic block
    Ret,    // Return from function
    Br,     // Unconditional branch
    Cbr,    // Conditional branch
    Switch, // Multi-way branch
    /// GNU computed goto: branch to the address in `src[0]`.
    ///
    /// The reachable blocks are not derivable from the instruction -- any
    /// label whose address was taken in this function may be the target -- so
    /// the CFG edges are recorded on the block, exactly as `asm goto` does.
    IndirectBr,

    // Integer arithmetic binary ops
    Add,
    Sub,
    Mul,
    DivU, // Unsigned division
    DivS, // Signed division
    ModU, // Unsigned modulo
    ModS, // Signed modulo
    Shl,  // Shift left
    Lsr,  // Logical shift right (unsigned)
    Asr,  // Arithmetic shift right (signed)

    // Floating-point binary ops
    FAdd,
    FSub,
    FMul,
    FDiv,

    // Bitwise/logical binary ops
    And,
    Or,
    Xor,

    // Integer comparisons (result is 0 or 1)
    SetEq, // ==
    SetNe, // !=
    SetLt, // < (signed)
    SetLe, // <= (signed)
    SetGt, // > (signed)
    SetGe, // >= (signed)
    SetB,  // < (unsigned, "below")
    SetBe, // <= (unsigned)
    SetA,  // > (unsigned, "above")
    SetAe, // >= (unsigned)

    // Floating-point comparisons. See [`FloatCmp`], which is how every
    // consumer should take one apart: the predicate, and for a relational
    // one whether a quiet NaN operand raises invalid.
    //
    // Quiet: raise nothing for a quiet NaN. `==`, `!=`, the <math.h>
    // comparison macros, and every comparison the compiler makes up for
    // itself (`isfinite`, `fpclassify`, the `sqrt` domain check).
    FCmpOEq,
    FCmpONe,
    FCmpOLt,
    FCmpOLe,
    FCmpOGt,
    FCmpOGe,
    // Signaling: raise invalid when either operand is a NaN. C's `<`, `<=`,
    // `>` and `>=` (C17 F.9.3, IEEE 754 5.11), and nothing else.
    FCmpsOLt,
    FCmpsOLe,
    FCmpsOGt,
    FCmpsOGe,

    // Unary ops
    Not,  // Bitwise NOT
    Neg,  // Integer negation
    FNeg, // Float negation
    // Float absolute value, at the width of `typ`: clears the sign bit and
    // nothing else, so it is exact and raises nothing, even for a NaN.
    Fabs,
    // `copysign`: src[0] with the sign bit of src[1], at the width of `typ`.
    // Only the sign bit changes, taken from a zero or a NaN as from anything
    // else, so it is exact and raises nothing, and a NaN keeps its payload.
    CopySign,
    // Square root of src[0], correctly rounded, at the width of `typ`: what
    // IEEE 754's squareRoot and every target's instruction compute. Sets no
    // `errno` -- the linearizer keeps a call for the arguments that must --
    // and `func_name` names the library function a target without the
    // instruction calls instead (see `arch::mapping::computes_in_place`).
    Sqrt,
    // `floor`, `ceil`, `trunc`, `round`, `rint` or `nearbyint` of src[0], at
    // the width of `typ`: the integer it rounds to, with its sign. Named in
    // `func_name` like `Sqrt`.
    RoundToIntegral(IntegralRounding),
    // `fmin` and `fmax` of src[0] and src[1], at the width of `typ`: a NaN
    // operand is ignored for the other. Named in `func_name` like `Sqrt`.
    FMin,
    FMax,
    // `fma`: src[0] * src[1] + src[2], rounded once, at the width of `typ`.
    // Named in `func_name` like `Sqrt`.
    Fma,

    // Type conversions
    Trunc, // Truncate to smaller integer
    Zext,  // Zero-extend to larger integer
    Sext,  // Sign-extend to larger integer
    FCvtU, // Float to unsigned int
    FCvtS, // Float to signed int
    UCvtF, // Unsigned int to float
    SCvtF, // Signed int to float
    FCvtF, // Float to float (different sizes)

    // Memory ops
    Load,  // Load from memory
    Store, // Store to memory

    // SSA-specific
    Phi,       // Phi node for SSA
    PhiSource, // Phi source: explicit defining instruction for phi operand in predecessor block
    Copy,      // Copy (for out-of-SSA)

    // Other
    SymAddr, // Get address of symbol
    /// Get the address of a *thread-local* symbol.
    ///
    /// Distinct from `SymAddr` because the address is not a link-time
    /// constant: under the dynamic model it is computed by a call, which
    /// clobbers a register. Carrying that in the opcode is what lets
    /// `opcode_constraints` declare the clobber without the register allocator
    /// needing the module's thread-local symbol set or the build mode -- it
    /// runs over the IR, long before either is in reach.
    TlsAddr,
    Call,   // Function call
    Select, // Ternary select: cond ? a : b (pure expressions only, enables cmov/csel)
    SetVal, // Create pseudo for constant
    Nop,    // No operation

    // Variadic function support
    VaStart, // Initialize va_list
    VaArg,   // Get next vararg
    VaEnd,   // Clean up va_list (usually no-op)
    VaCopy,
    /// `__builtin_va_arg_pack_len()`: how many variadic arguments the caller
    /// passed. Replaced with a constant when the enclosing `always_inline`
    /// function is inlined; a survivor is diagnosed, never emitted.
    VaArgPackLen, // Copy va_list
    /// `__builtin_constant_p`, deferred until propagation has run.
    ///
    /// Resolved by `sccp` when it proves the operand constant, and by
    /// `ir::lower` to 0 otherwise -- which is every case at `-O0`, where
    /// the optimizer does not run at all.
    ConstantP,

    // Byte-swapping builtins
    Bswap16, // Byte-swap 16-bit value
    Bswap32, // Byte-swap 32-bit value
    Bswap64, // Byte-swap 64-bit value

    // Count trailing zeros builtins
    Ctz32, // Count trailing zeros in 32-bit value
    Ctz64, // Count trailing zeros in 64-bit value

    // Count leading zeros builtins
    Clz32, // Count leading zeros in 32-bit value
    Clz64, // Count leading zeros in 64-bit value

    // Population count builtins
    Popcount32, // Count set bits in 32-bit value
    Popcount64, // Count set bits in 64-bit value

    // Stack allocation builtin
    Alloca, // Dynamic stack allocation
    /// Capture the stack pointer, so a later [`Opcode::StackRestore`] can put
    /// it back. Defines a pointer-sized pseudo and reads nothing.
    ///
    /// A VLA's scope is bracketed by the pair, and the program can write them
    /// itself as `__builtin_stack_save` / `__builtin_stack_restore`. They also
    /// exist for inlining. A call to a function that `alloca`s releases that
    /// memory when it returns; splicing the body into the caller would instead
    /// hold it until the *caller* returns, so `for (...) use(n)` with an
    /// `alloca` in `use` would grow the stack every iteration until it
    /// overflowed. Bracketing the inlined body restores the call's lifetime.
    StackSave,
    /// Put the stack pointer back to what a [`Opcode::StackSave`] captured.
    StackRestore,

    // Memory builtins - generate calls to C library functions
    Memset,  // memset(dest, c, n) - set memory
    Memcpy,  // memcpy(dest, src, n) - copy memory
    Memmove, // memmove(dest, src, n) - copy overlapping memory

    // Floating-point builtins
    // `signbit`: whether the sign bit of the operand is set, as 0 or 1 --
    // for `-0.0` and a NaN too. Shaped like a conversion: `typ`/`size` are
    // the `int` result, `src_typ`/`src_size` the floating operand.
    Signbit,

    // Optimization hints
    Unreachable, // Code path is never reached (undefined behavior if reached)

    // Stack introspection
    FrameAddress, // __builtin_frame_address(level) - frame pointer `frame_level()` frames up
    ReturnAddress, // __builtin_return_address(level) - return address `frame_level()` frames up

    // Non-local jumps (setjmp/longjmp)
    Setjmp,  // Save execution context, returns 0 or value from longjmp
    Longjmp, // Restore execution context (never returns)

    // Inline assembly
    Asm, // Inline assembly statement

    // Atomic memory operations (C11 _Atomic support)
    AtomicLoad,     // Atomic load with memory ordering
    AtomicStore,    // Atomic store with memory ordering
    AtomicSwap,     // Atomic exchange (returns old value)
    AtomicCas,      // Compare-and-swap (returns success/old value)
    AtomicFetchAdd, // Atomic fetch-and-add
    AtomicFetchSub, // Atomic fetch-and-subtract
    AtomicFetchAnd, // Atomic fetch-and-and
    AtomicFetchOr,  // Atomic fetch-and-or
    AtomicFetchXor, // Atomic fetch-and-xor
    Fence,          // Memory fence

    // Int128 decomposition ops (used by mapping pass expansion)
    Lo64,   // Extract low 64 bits from 128-bit pseudo
    Hi64,   // Extract high 64 bits from 128-bit pseudo
    Pair64, // Combine two 64-bit pseudos into 128-bit: target = (src[0]=lo, src[1]=hi)
    AddC,   // 64-bit add with carry output: target = src[0] + src[1], sets carry
    AdcC, // 64-bit add with carry in+out: target = src[0] + src[1] + carry; src[2] = carry producer
    SubC, // 64-bit sub with borrow output: target = src[0] - src[1], sets borrow
    SbcC, // 64-bit sub with borrow in+out: target = src[0] - src[1] - borrow; src[2] = borrow producer
    UMulHi, // Upper 64 bits of unsigned 64×64 multiply: target = (src[0] * src[1]) >> 64
    /// The lifetime of the local `InsnExtra::lifetime_of` names ends here:
    /// control falls out of the block that declared it (C17 6.2.4p6). Named
    /// out of band, never in `src`, so no analysis counts it as a use, an
    /// escape or a reason to keep the local. See `arch::regalloc::LocalLifetimes`.
    LifetimeEnd,
    /// A lane-wise operation on whole GNU vectors, computed by the target's
    /// packed instructions: `typ` is the vector type, which says the lanes,
    /// and `size` its width, 128 or 64. Each operand and the result is the
    /// vector's bits, held as its register-sized carrier (a binary128 or a
    /// `double`). Built only for what `arch::simd::native` lists for the
    /// target; anything else is computed lane by lane.
    Simd(SimdOp),
}

/// The lane-wise operation of an [`Opcode::Simd`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SimdOp {
    /// Integer lanes: src[0] + src[1], wrapping.
    Add,
    /// Integer lanes: src[0] - src[1], wrapping.
    Sub,
    /// Integer lanes, bitwise.
    And,
    Or,
    Xor,
    /// Integer lanes: ~src[0].
    Not,
    /// Integer lanes: -src[0], wrapping.
    Neg,
    /// Floating lanes, IEEE.
    FAdd,
    FSub,
    FMul,
    FDiv,
    /// Floating lanes: src[0] with each sign bit flipped.
    FNeg,
    /// Every lane src[0], a scalar of the lane type.
    Splat,
    /// Integer lanes: src[0] * src[1], wrapping.
    Mul,
    /// Integer lanes: src[0] shifted by the count in the same lane of
    /// src[1] -- left, right logically, right arithmetically.
    Shl,
    Lsr,
    Asr,
    /// Integer lanes: every lane of src[0] shifted by src[1], one scalar
    /// count of the lane type.
    ShlScalar,
    LsrScalar,
    AsrScalar,
    /// Integer lanes compared, each lane of the result all ones where it
    /// holds and zero where not: ==, !=, signed > and >=, unsigned > and
    /// >=. A < or <= is the > or >= of the operands swapped.
    CmpEq,
    CmpNe,
    CmpGt,
    CmpGe,
    CmpGtU,
    CmpGeU,
    /// Floating lanes compared, as C does: != holds for an unordered pair,
    /// the others do not.
    FCmpEq,
    FCmpNe,
    FCmpGt,
    FCmpGe,
    /// The lanes of src[0], followed by src[1]'s when there are two, picked
    /// by the constant indices in `InsnExtra::shuffle`: result lane k is
    /// lane `indices.lane(k)` of the two, or anything for an unspecified
    /// one. Out of line, so every instruction does not pay for sixteen.
    Shuffle,
    /// Each lane converted, at the same width: signed or unsigned integer
    /// to floating, and floating to signed or unsigned integer, truncated.
    /// `typ` is the result's vector type.
    CvtSF,
    CvtUF,
    CvtFS,
    CvtFU,
}

/// The constant lane indices of a [`SimdOp::Shuffle`]: one per result
/// lane, into the lanes of its operands in order, or
/// [`ShuffleIndices::UNSPECIFIED`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShuffleIndices {
    lanes: [u8; 16],
    count: u8,
}

impl ShuffleIndices {
    /// A result lane whose value may be anything (`-1` to
    /// `__builtin_shufflevector`).
    pub const UNSPECIFIED: u8 = u8::MAX;

    /// The indices of `lanes`, at most sixteen, each an operand lane or
    /// `None` for an unspecified one.
    pub fn new(lanes: &[Option<u32>]) -> Self {
        assert!(lanes.len() <= 16, "a shuffle of more than 16 lanes");
        let mut out = [Self::UNSPECIFIED; 16];
        for (slot, lane) in out.iter_mut().zip(lanes) {
            if let Some(i) = lane {
                *slot = u8::try_from(*i).expect("a lane index below 32");
            }
        }
        Self {
            lanes: out,
            count: lanes.len() as u8,
        }
    }

    /// The operand lane of result lane `k`, or `None` when unspecified.
    pub fn lane(&self, k: usize) -> Option<usize> {
        let i = self.lanes[..self.count as usize][k];
        (i != Self::UNSPECIFIED).then_some(i as usize)
    }

    /// The number of result lanes.
    pub fn len(&self) -> usize {
        self.count as usize
    }

    /// Whether there are no lanes; never, for a shuffle of a vector.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }
}

impl SimdOp {
    /// Whether this lane-wise operation can raise a floating-point
    /// exception, lane by lane as [`Opcode::fp_raise`] answers the scalar
    /// one. The ordered lane comparisons are the signaling ones both targets
    /// emit (`cmpltps`, `fcmgt`); a lane conversion is `int` to `float` or
    /// back, which is inexact or invalid for some lane values.
    pub fn fp_raise(self) -> FpRaise {
        use SimdOp::*;
        match self {
            FAdd | FSub | FMul | FDiv | FCmpGt | FCmpGe | CvtSF | CvtUF | CvtFS | CvtFU => {
                FpRaise::May
            }
            Add | Sub | And | Or | Xor | Not | Neg | FNeg | Splat | Mul | Shl | Lsr | Asr
            | ShlScalar | LsrScalar | AsrScalar | CmpEq | CmpNe | CmpGt | CmpGe | CmpGtU
            | CmpGeU | FCmpEq | FCmpNe | Shuffle => FpRaise::Never,
        }
    }

    /// Every operation, for tests over the whole set.
    pub const ALL: [SimdOp; 35] = [
        SimdOp::Add,
        SimdOp::Sub,
        SimdOp::And,
        SimdOp::Or,
        SimdOp::Xor,
        SimdOp::Not,
        SimdOp::Neg,
        SimdOp::FAdd,
        SimdOp::FSub,
        SimdOp::FMul,
        SimdOp::FDiv,
        SimdOp::FNeg,
        SimdOp::Splat,
        SimdOp::Mul,
        SimdOp::Shl,
        SimdOp::Lsr,
        SimdOp::Asr,
        SimdOp::ShlScalar,
        SimdOp::LsrScalar,
        SimdOp::AsrScalar,
        SimdOp::CmpEq,
        SimdOp::CmpNe,
        SimdOp::CmpGt,
        SimdOp::CmpGe,
        SimdOp::CmpGtU,
        SimdOp::CmpGeU,
        SimdOp::FCmpEq,
        SimdOp::FCmpNe,
        SimdOp::FCmpGt,
        SimdOp::FCmpGe,
        SimdOp::Shuffle,
        SimdOp::CvtSF,
        SimdOp::CvtUF,
        SimdOp::CvtFS,
        SimdOp::CvtFU,
    ];

    /// Whether the operation takes one operand.
    pub fn is_unary(self) -> bool {
        matches!(
            self,
            SimdOp::Not
                | SimdOp::Neg
                | SimdOp::FNeg
                | SimdOp::Splat
                | SimdOp::CvtSF
                | SimdOp::CvtUF
                | SimdOp::CvtFS
                | SimdOp::CvtFU
        )
    }

    /// The lanes it applies to: `Some(true)` floating only, `Some(false)`
    /// integer only, `None` either.
    pub fn float_lanes(self) -> Option<bool> {
        match self {
            SimdOp::FAdd
            | SimdOp::FSub
            | SimdOp::FMul
            | SimdOp::FDiv
            | SimdOp::FNeg
            | SimdOp::FCmpEq
            | SimdOp::FCmpNe
            | SimdOp::FCmpGt
            | SimdOp::FCmpGe
            | SimdOp::CvtSF
            | SimdOp::CvtUF => Some(true),
            SimdOp::Splat | SimdOp::Shuffle => None,
            _ => Some(false),
        }
    }

    /// The form shifting every lane by one scalar count, of a shift by
    /// per-lane counts.
    pub fn by_scalar(self) -> Option<SimdOp> {
        match self {
            SimdOp::Shl => Some(SimdOp::ShlScalar),
            SimdOp::Lsr => Some(SimdOp::LsrScalar),
            SimdOp::Asr => Some(SimdOp::AsrScalar),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            SimdOp::Add => "vadd",
            SimdOp::Sub => "vsub",
            SimdOp::And => "vand",
            SimdOp::Or => "vor",
            SimdOp::Xor => "vxor",
            SimdOp::Not => "vnot",
            SimdOp::Neg => "vneg",
            SimdOp::FAdd => "vfadd",
            SimdOp::FSub => "vfsub",
            SimdOp::FMul => "vfmul",
            SimdOp::FDiv => "vfdiv",
            SimdOp::FNeg => "vfneg",
            SimdOp::Splat => "vsplat",
            SimdOp::Mul => "vmul",
            SimdOp::Shl => "vshl",
            SimdOp::Lsr => "vlsr",
            SimdOp::Asr => "vasr",
            SimdOp::ShlScalar => "vshl_s",
            SimdOp::LsrScalar => "vlsr_s",
            SimdOp::AsrScalar => "vasr_s",
            SimdOp::CmpEq => "vcmp_eq",
            SimdOp::CmpNe => "vcmp_ne",
            SimdOp::CmpGt => "vcmp_gt",
            SimdOp::CmpGe => "vcmp_ge",
            SimdOp::CmpGtU => "vcmp_gtu",
            SimdOp::CmpGeU => "vcmp_geu",
            SimdOp::FCmpEq => "vfcmp_eq",
            SimdOp::FCmpNe => "vfcmp_ne",
            SimdOp::FCmpGt => "vfcmp_gt",
            SimdOp::FCmpGe => "vfcmp_ge",
            SimdOp::Shuffle => "vshuffle",
            SimdOp::CvtSF => "vcvt_sf",
            SimdOp::CvtUF => "vcvt_uf",
            SimdOp::CvtFS => "vcvt_fs",
            SimdOp::CvtFU => "vcvt_fu",
        }
    }
}

impl Opcode {
    /// Integer binary arithmetic: what `constfold::eval_binop` evaluates
    /// besides the comparisons.
    pub fn is_int_arith(self) -> bool {
        matches!(
            self,
            Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::DivS
                | Opcode::DivU
                | Opcode::ModS
                | Opcode::ModU
                | Opcode::Shl
                | Opcode::Lsr
                | Opcode::Asr
                | Opcode::And
                | Opcode::Or
                | Opcode::Xor
        )
    }

    /// An integer comparison: `seteq` .. `setae`, a 0-or-1 result.
    pub fn is_int_comparison(self) -> bool {
        matches!(
            self,
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
        )
    }

    /// A floating comparison, quiet or signaling, with a 0-or-1 result.
    pub fn is_float_comparison(self) -> bool {
        self.float_cmp().is_some()
    }

    /// The floating comparison this opcode is, taken apart: its predicate
    /// and, for a relational one, what a quiet NaN operand does. `None` for
    /// every other opcode.
    pub fn float_cmp(self) -> Option<FloatCmp> {
        use FloatCmp::*;
        use NanCompare::*;
        Some(match self {
            Opcode::FCmpOEq => Eq,
            Opcode::FCmpONe => Ne,
            Opcode::FCmpOLt => Lt(Quiet),
            Opcode::FCmpOLe => Le(Quiet),
            Opcode::FCmpOGt => Gt(Quiet),
            Opcode::FCmpOGe => Ge(Quiet),
            Opcode::FCmpsOLt => Lt(Signaling),
            Opcode::FCmpsOLe => Le(Signaling),
            Opcode::FCmpsOGt => Gt(Signaling),
            Opcode::FCmpsOGe => Ge(Signaling),
            _ => return None,
        })
    }

    /// Whether this operation can raise a floating-point exception; see
    /// [`FpRaise`]. Exhaustive, so a new opcode is classified when it is
    /// added rather than defaulted.
    pub fn fp_raise(self) -> FpRaise {
        use Opcode::*;
        match self {
            FAdd | FSub | FMul | FDiv | Sqrt | RoundToIntegral(_) | FMin | FMax | Fma | Call
            | Asm => FpRaise::May,
            FCvtU | FCvtS | UCvtF | SCvtF | FCvtF => FpRaise::Conversion,
            FCmpOEq | FCmpONe | FCmpOLt | FCmpOLe | FCmpOGt | FCmpOGe | FCmpsOLt | FCmpsOLe
            | FCmpsOGt | FCmpsOGe => match self.float_cmp().map(FloatCmp::nan) {
                Some(NanCompare::Signaling) => FpRaise::May,
                _ => FpRaise::Never,
            },
            Simd(op) => op.fp_raise(),
            FNeg | Fabs | CopySign | Signbit => FpRaise::Never,
            Entry | Ret | Br | Cbr | Switch | IndirectBr | Add | Sub | Mul | DivU | DivS | ModU
            | ModS | Shl | Lsr | Asr | And | Or | Xor | SetEq | SetNe | SetLt | SetLe | SetGt
            | SetGe | SetB | SetBe | SetA | SetAe | Not | Neg | Trunc | Zext | Sext | Load
            | Store | Phi | PhiSource | Copy | SymAddr | TlsAddr | Select | SetVal | Nop
            | VaStart | VaArg | VaEnd | VaCopy | VaArgPackLen | ConstantP | Bswap16 | Bswap32
            | Bswap64 | Ctz32 | Ctz64 | Clz32 | Clz64 | Popcount32 | Popcount64 | Alloca
            | StackSave | StackRestore | Memset | Memcpy | Memmove | Unreachable | FrameAddress
            | ReturnAddress | Setjmp | Longjmp | AtomicLoad | AtomicStore | AtomicSwap
            | AtomicCas | AtomicFetchAdd | AtomicFetchSub | AtomicFetchAnd | AtomicFetchOr
            | AtomicFetchXor | Fence | Lo64 | Hi64 | Pair64 | AddC | AdcC | SubC | SbcC
            | UMulHi | LifetimeEnd => FpRaise::Never,
        }
    }

    /// Any comparison, integer or floating.
    pub fn is_comparison(self) -> bool {
        self.is_int_comparison() || self.is_float_comparison()
    }

    /// Does this opcode read operands of a type other than its result's --
    /// recorded in `src_typ`/`src_size` while `typ`/`size` describe the
    /// result, as for every opcode?
    ///
    /// The conversions, the comparisons (which read any type and produce an
    /// `int` or a `_Bool`), the bit counts (which count a 32- or 64-bit
    /// operand into an `int`), and `Signbit` (which tests a floating operand
    /// into an `int`).
    ///
    /// Every one records a nonzero `src_size`, which the validator checks
    /// (I10), so [`Instruction::operand_width`] is never an unknown 0.
    pub fn reads_another_type(self) -> bool {
        self.is_comparison()
            || self.is_conversion()
            || self.is_bit_count()
            || self == Opcode::Signbit
    }

    /// The conversions: an integer width change (`Sext`, `Zext`, `Trunc`)
    /// or a change between an integer and a floating type, or between two
    /// floating types.
    pub fn is_conversion(self) -> bool {
        self.is_int_width_change()
            || matches!(
                self,
                Opcode::FCvtS | Opcode::FCvtU | Opcode::SCvtF | Opcode::UCvtF | Opcode::FCvtF
            )
    }

    /// The integer width changes. An extension reads a narrower operand
    /// than its result and a truncation a wider one -- never the same
    /// width, which is no conversion at all -- and the validator checks it
    /// (I12).
    pub fn is_int_width_change(self) -> bool {
        matches!(self, Opcode::Sext | Opcode::Zext | Opcode::Trunc)
    }

    /// The bit counts: an `int` count of the bits of a 32- or 64-bit operand.
    pub fn is_bit_count(self) -> bool {
        matches!(
            self,
            Opcode::Ctz32
                | Opcode::Ctz64
                | Opcode::Clz32
                | Opcode::Clz64
                | Opcode::Popcount32
                | Opcode::Popcount64
        )
    }

    /// Check if this opcode is a terminator (ends a basic block)
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Opcode::Ret
                | Opcode::Br
                | Opcode::Cbr
                | Opcode::Switch
                | Opcode::IndirectBr
                | Opcode::Unreachable
                | Opcode::Longjmp
        )
    }

    /// Could an instruction with this opcode read or write memory?
    ///
    /// The companion to [`Instruction::is_memory_barrier`], which answers
    /// *ordering*: this answers *extent*. A pass that removes or moves a
    /// memory operation must consult this to know what lies between two
    /// points, and `is_memory_barrier` to know what it may cross.
    ///
    /// Everything that might touch memory is named. [`Self::has_side_effects`]
    /// is derived from this set, so an opcode added here is a DCE root unless
    /// it is a `Load`.
    pub fn may_access_memory(&self) -> bool {
        matches!(
            self,
            Opcode::Load
                | Opcode::Store
                | Opcode::Call
                | Opcode::Memset
                | Opcode::Memcpy
                | Opcode::Memmove
                | Opcode::VaStart
                | Opcode::VaArg
                | Opcode::VaCopy
                | Opcode::VaEnd
                | Opcode::Alloca
                | Opcode::StackSave
                | Opcode::StackRestore
                | Opcode::Setjmp
                | Opcode::Longjmp
                | Opcode::Asm
                | Opcode::Fence
        ) || self.is_atomic()
    }

    /// Whether this opcode computes a libm function, which its instruction
    /// names in `func_name` for a target that calls the function instead.
    pub fn is_libm(&self) -> bool {
        matches!(
            self,
            Opcode::Sqrt | Opcode::RoundToIntegral(_) | Opcode::FMin | Opcode::FMax | Opcode::Fma
        )
    }

    /// Check if this opcode has side effects (cannot be deleted even if unused).
    /// These are "root" instructions for dead code elimination.
    ///
    /// Derived from the other predicates, so that a memory access or a memory
    /// barrier (every barrier opcode reaches memory) is a root by construction.
    pub fn has_side_effects(&self) -> bool {
        self.is_terminator()
            // A read of non-volatile memory has no effect, so DCE may delete
            // a `Load`; a volatile one is kept per access, by
            // `Instruction::is_volatile_access`, which `dce::is_root` consults.
            || (self.may_access_memory() && *self != Opcode::Load)
            // `Entry` marks where the function begins. `LifetimeEnd` is not
            // an effect of the program's, but a fact the allocator reads,
            // which deleting would lose.
            || matches!(self, Opcode::Entry | Opcode::LifetimeEnd)
    }

    /// True for the atomic memory operations (not `Fence`, which touches no
    /// address and needs no scratch registers).
    ///
    /// Both backends lower these through fixed scratch registers that are in
    /// the allocatable pool, so the register allocator has to know.
    pub fn is_atomic(self) -> bool {
        matches!(
            self,
            Opcode::AtomicLoad
                | Opcode::AtomicStore
                | Opcode::AtomicSwap
                | Opcode::AtomicCas
                | Opcode::AtomicFetchAdd
                | Opcode::AtomicFetchSub
                | Opcode::AtomicFetchAnd
                | Opcode::AtomicFetchOr
                | Opcode::AtomicFetchXor
        )
    }

    /// True for the memory accesses a backend addresses as `src[0] + offset`:
    /// plain loads and stores, and the atomic operations.
    pub fn addresses_memory(self) -> bool {
        matches!(self, Opcode::Load | Opcode::Store) || self.is_atomic()
    }

    /// Get the opcode name for display
    pub fn name(&self) -> &'static str {
        match self {
            Opcode::Entry => "entry",
            Opcode::Ret => "ret",
            Opcode::Br => "br",
            Opcode::Cbr => "cbr",
            Opcode::Switch => "switch",
            Opcode::IndirectBr => "indirectbr",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::DivU => "divu",
            Opcode::DivS => "divs",
            Opcode::ModU => "modu",
            Opcode::ModS => "mods",
            Opcode::Shl => "shl",
            Opcode::Lsr => "lsr",
            Opcode::Asr => "asr",
            Opcode::FAdd => "fadd",
            Opcode::FSub => "fsub",
            Opcode::FMul => "fmul",
            Opcode::FDiv => "fdiv",
            Opcode::And => "and",
            Opcode::Or => "or",
            Opcode::Xor => "xor",
            Opcode::SetEq => "seteq",
            Opcode::SetNe => "setne",
            Opcode::SetLt => "setlt",
            Opcode::SetLe => "setle",
            Opcode::SetGt => "setgt",
            Opcode::SetGe => "setge",
            Opcode::SetB => "setb",
            Opcode::SetBe => "setbe",
            Opcode::SetA => "seta",
            Opcode::SetAe => "setae",
            Opcode::FCmpOEq => "fcmp_oeq",
            Opcode::FCmpONe => "fcmp_one",
            Opcode::FCmpOLt => "fcmp_olt",
            Opcode::FCmpOLe => "fcmp_ole",
            Opcode::FCmpOGt => "fcmp_ogt",
            Opcode::FCmpOGe => "fcmp_oge",
            Opcode::FCmpsOLt => "fcmps_olt",
            Opcode::FCmpsOLe => "fcmps_ole",
            Opcode::FCmpsOGt => "fcmps_ogt",
            Opcode::FCmpsOGe => "fcmps_oge",
            Opcode::Not => "not",
            Opcode::Neg => "neg",
            Opcode::FNeg => "fneg",
            Opcode::Fabs => "fabs",
            Opcode::CopySign => "copysign",
            Opcode::Sqrt => "sqrt",
            Opcode::FMin => "fmin",
            Opcode::FMax => "fmax",
            Opcode::Fma => "fma",
            Opcode::Simd(op) => op.name(),
            Opcode::RoundToIntegral(how) => match how {
                IntegralRounding::Floor => "ffloor",
                IntegralRounding::Ceil => "fceil",
                IntegralRounding::Trunc => "ftrunc",
                IntegralRounding::Round => "fround",
                IntegralRounding::Rint => "frint",
                IntegralRounding::NearbyInt => "fnearbyint",
            },
            Opcode::Trunc => "trunc",
            Opcode::Zext => "zext",
            Opcode::Sext => "sext",
            Opcode::FCvtU => "fcvtu",
            Opcode::FCvtS => "fcvts",
            Opcode::UCvtF => "ucvtf",
            Opcode::SCvtF => "scvtf",
            Opcode::FCvtF => "fcvtf",
            Opcode::Load => "load",
            Opcode::Store => "store",
            Opcode::Phi => "phi",
            Opcode::PhiSource => "phisrc",
            Opcode::Copy => "copy",
            Opcode::SymAddr => "symaddr",
            Opcode::TlsAddr => "tlsaddr",
            Opcode::Call => "call",
            Opcode::Select => "sel",
            Opcode::SetVal => "setval",
            Opcode::Nop => "nop",
            Opcode::VaStart => "va_start",
            Opcode::VaArg => "va_arg",
            Opcode::VaEnd => "va_end",
            Opcode::VaCopy => "va_copy",
            Opcode::VaArgPackLen => "va_arg_pack_len",
            Opcode::ConstantP => "constant_p",
            Opcode::Bswap16 => "bswap16",
            Opcode::Bswap32 => "bswap32",
            Opcode::Bswap64 => "bswap64",
            Opcode::Ctz32 => "ctz32",
            Opcode::Ctz64 => "ctz64",
            Opcode::Clz32 => "clz32",
            Opcode::Clz64 => "clz64",
            Opcode::Popcount32 => "popcount32",
            Opcode::Popcount64 => "popcount64",
            Opcode::Alloca => "alloca",
            Opcode::StackSave => "stacksave",
            Opcode::StackRestore => "stackrestore",
            Opcode::Memset => "memset",
            Opcode::Memcpy => "memcpy",
            Opcode::Memmove => "memmove",
            Opcode::Signbit => "signbit",
            Opcode::Unreachable => "unreachable",
            Opcode::FrameAddress => "frame_address",
            Opcode::ReturnAddress => "return_address",
            Opcode::Setjmp => "setjmp",
            Opcode::Longjmp => "longjmp",
            Opcode::Asm => "asm",
            Opcode::AtomicLoad => "atomic_load",
            Opcode::AtomicStore => "atomic_store",
            Opcode::AtomicSwap => "atomic_swap",
            Opcode::AtomicCas => "atomic_cas",
            Opcode::AtomicFetchAdd => "atomic_fetch_add",
            Opcode::AtomicFetchSub => "atomic_fetch_sub",
            Opcode::AtomicFetchAnd => "atomic_fetch_and",
            Opcode::AtomicFetchOr => "atomic_fetch_or",
            Opcode::AtomicFetchXor => "atomic_fetch_xor",
            Opcode::Fence => "fence",
            Opcode::Lo64 => "lo64",
            Opcode::Hi64 => "hi64",
            Opcode::Pair64 => "pair64",
            Opcode::AddC => "addc",
            Opcode::AdcC => "adcc",
            Opcode::SubC => "subc",
            Opcode::SbcC => "sbcc",
            Opcode::UMulHi => "umulhi",
            Opcode::LifetimeEnd => "lifetime.end",
        }
    }
}

/// Declares [`Opcode::ALL`] and [`Opcode::is_listed`] from one list of the
/// payload-free variants, so the two cannot disagree: `is_listed` matches
/// without a wildcard, and a variant added to `Opcode` or to
/// `IntegralRounding` does not compile until it is listed here, which also
/// puts it in `ALL`.
#[cfg(test)]
macro_rules! every_opcode {
    ($($op:ident),* $(,)?) => {
        impl Opcode {
            /// Every opcode, with `RoundToIntegral` once per rounding, for a
            /// test that checks a property of the whole opcode table.
            pub(crate) const ALL: &'static [Opcode] = &[
                $(Opcode::$op,)*
                Opcode::RoundToIntegral(IntegralRounding::Floor),
                Opcode::RoundToIntegral(IntegralRounding::Ceil),
                Opcode::RoundToIntegral(IntegralRounding::Trunc),
                Opcode::RoundToIntegral(IntegralRounding::Round),
                Opcode::RoundToIntegral(IntegralRounding::Rint),
                Opcode::RoundToIntegral(IntegralRounding::NearbyInt),
                Opcode::Simd(SimdOp::Add),
                Opcode::Simd(SimdOp::Sub),
                Opcode::Simd(SimdOp::And),
                Opcode::Simd(SimdOp::Or),
                Opcode::Simd(SimdOp::Xor),
                Opcode::Simd(SimdOp::Not),
                Opcode::Simd(SimdOp::Neg),
                Opcode::Simd(SimdOp::FAdd),
                Opcode::Simd(SimdOp::FSub),
                Opcode::Simd(SimdOp::FMul),
                Opcode::Simd(SimdOp::FDiv),
                Opcode::Simd(SimdOp::FNeg),
                Opcode::Simd(SimdOp::Splat),
                Opcode::Simd(SimdOp::Mul),
                Opcode::Simd(SimdOp::Shl),
                Opcode::Simd(SimdOp::Lsr),
                Opcode::Simd(SimdOp::Asr),
                Opcode::Simd(SimdOp::ShlScalar),
                Opcode::Simd(SimdOp::LsrScalar),
                Opcode::Simd(SimdOp::AsrScalar),
                Opcode::Simd(SimdOp::CmpEq),
                Opcode::Simd(SimdOp::CmpNe),
                Opcode::Simd(SimdOp::CmpGt),
                Opcode::Simd(SimdOp::CmpGe),
                Opcode::Simd(SimdOp::CmpGtU),
                Opcode::Simd(SimdOp::CmpGeU),
                Opcode::Simd(SimdOp::FCmpEq),
                Opcode::Simd(SimdOp::FCmpNe),
                Opcode::Simd(SimdOp::FCmpGt),
                Opcode::Simd(SimdOp::FCmpGe),
                Opcode::Simd(SimdOp::Shuffle),
                Opcode::Simd(SimdOp::CvtSF),
                Opcode::Simd(SimdOp::CvtUF),
                Opcode::Simd(SimdOp::CvtFS),
                Opcode::Simd(SimdOp::CvtFU),
            ];

            /// The exhaustiveness guard behind [`Opcode::ALL`]; always true.
            fn is_listed(self) -> bool {
                match self {
                    $(Opcode::$op)|* => true,
                    Opcode::RoundToIntegral(
                        IntegralRounding::Floor
                        | IntegralRounding::Ceil
                        | IntegralRounding::Trunc
                        | IntegralRounding::Round
                        | IntegralRounding::Rint
                        | IntegralRounding::NearbyInt,
                    ) => true,
                    Opcode::Simd(
                        SimdOp::Add
                        | SimdOp::Sub
                        | SimdOp::And
                        | SimdOp::Or
                        | SimdOp::Xor
                        | SimdOp::Not
                        | SimdOp::Neg
                        | SimdOp::FAdd
                        | SimdOp::FSub
                        | SimdOp::FMul
                        | SimdOp::FDiv
                        | SimdOp::FNeg
                        | SimdOp::Splat
                        | SimdOp::Mul
                        | SimdOp::Shl
                        | SimdOp::Lsr
                        | SimdOp::Asr
                        | SimdOp::ShlScalar
                        | SimdOp::LsrScalar
                        | SimdOp::AsrScalar
                        | SimdOp::CmpEq
                        | SimdOp::CmpNe
                        | SimdOp::CmpGt
                        | SimdOp::CmpGe
                        | SimdOp::CmpGtU
                        | SimdOp::CmpGeU
                        | SimdOp::FCmpEq
                        | SimdOp::FCmpNe
                        | SimdOp::FCmpGt
                        | SimdOp::FCmpGe
                        | SimdOp::Shuffle
                        | SimdOp::CvtSF
                        | SimdOp::CvtUF
                        | SimdOp::CvtFS
                        | SimdOp::CvtFU,
                    ) => true,
                }
            }
        }
    };
}

#[cfg(test)]
every_opcode! {
    Entry, Ret, Br, Cbr, Switch, IndirectBr, Add, Sub, Mul, DivU, DivS, ModU, ModS, Shl, Lsr, Asr,
    FAdd, FSub, FMul, FDiv, And, Or, Xor, SetEq, SetNe, SetLt, SetLe, SetGt, SetGe, SetB, SetBe,
    SetA, SetAe, FCmpOEq, FCmpONe, FCmpOLt, FCmpOLe, FCmpOGt, FCmpOGe, FCmpsOLt, FCmpsOLe, FCmpsOGt,
    FCmpsOGe, Not, Neg, FNeg, Fabs,
    CopySign, Sqrt, FMin, FMax, Fma, Trunc, Zext, Sext, FCvtU, FCvtS, UCvtF, SCvtF, FCvtF, Load,
    Store, Phi, PhiSource, Copy, SymAddr, TlsAddr, Call, Select, SetVal, Nop, VaStart, VaArg,
    VaEnd, VaCopy, VaArgPackLen, ConstantP, Bswap16, Bswap32, Bswap64, Ctz32, Ctz64, Clz32, Clz64,
    Popcount32, Popcount64, Alloca, StackSave, StackRestore, Memset, Memcpy, Memmove, Signbit,
    Unreachable, FrameAddress, ReturnAddress, Setjmp, Longjmp, Asm, AtomicLoad, AtomicStore,
    AtomicSwap, AtomicCas, AtomicFetchAdd, AtomicFetchSub, AtomicFetchAnd, AtomicFetchOr,
    AtomicFetchXor, Fence, Lo64, Hi64, Pair64, AddC, AdcC, SubC, SbcC, UMulHi, LifetimeEnd,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

// Memory Ordering - for atomic operations

/// Memory ordering for atomic operations (C11 memory model)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum MemoryOrder {
    /// No ordering constraints
    #[default]
    Relaxed = 0,
    /// Data dependency ordering (rarely used, treated as Acquire)
    Consume = 1,
    /// Acquire semantics: no reads/writes can be reordered before this
    Acquire = 2,
    /// Release semantics: no reads/writes can be reordered after this
    Release = 3,
    /// Both acquire and release semantics
    AcqRel = 4,
    /// Sequential consistency: total global ordering
    SeqCst = 5,
}

impl MemoryOrder {
    /// The order a `__ATOMIC_*` value names, or `None` outside 0..=5.
    pub fn from_value(value: i128) -> Option<Self> {
        Some(match value {
            0 => Self::Relaxed,
            1 => Self::Consume,
            2 => Self::Acquire,
            3 => Self::Release,
            4 => Self::AcqRel,
            5 => Self::SeqCst,
            _ => return None,
        })
    }

    /// True when a load under this order is an acquire. `consume` is
    /// one: no compiler tracks the dependencies it would need, and gcc
    /// promotes it the same way.
    pub fn acquires(self) -> bool {
        matches!(
            self,
            Self::Consume | Self::Acquire | Self::AcqRel | Self::SeqCst
        )
    }

    /// True when a store under this order is a release.
    pub fn releases(self) -> bool {
        matches!(self, Self::Release | Self::AcqRel | Self::SeqCst)
    }
}

/// Whom a `Fence` orders against.
///
/// Both are the same compiler barrier to every IR pass, which asks the
/// opcode; only the code a back end emits differs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FenceScope {
    /// `atomic_thread_fence`: other threads, so the hardware must order too.
    #[default]
    Thread,
    /// `atomic_signal_fence`: a signal handler on this same thread, which
    /// sees the thread's own accesses in program order. Ordering the
    /// compiler is all it needs, so it costs no instruction.
    Signal,
}

impl fmt::Display for MemoryOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemoryOrder::Relaxed => write!(f, "relaxed"),
            MemoryOrder::Consume => write!(f, "consume"),
            MemoryOrder::Acquire => write!(f, "acquire"),
            MemoryOrder::Release => write!(f, "release"),
            MemoryOrder::AcqRel => write!(f, "acq_rel"),
            MemoryOrder::SeqCst => write!(f, "seq_cst"),
        }
    }
}

// Pseudo - Virtual registers / values in SSA form

/// Unique ID for a pseudo (virtual register)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct PseudoId(pub u32);

impl fmt::Display for PseudoId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// Type of pseudo value
#[derive(Debug, Clone, PartialEq, Default)]
pub enum PseudoKind {
    /// Void (no value)
    #[default]
    Void,
    /// Undefined value
    Undef,
    /// Virtual register (result of an instruction)
    Reg(u32),
    /// Function argument
    Arg(u32),
    /// Phi node result
    Phi(u32),
    /// Symbol reference (variable, function)
    Sym(String),
    /// Constant integer value
    Val(i128),
    /// Constant float value
    FVal(FloatVal),
}

/// A pseudo (virtual register or value) in SSA form
#[derive(Debug, Clone, Default)]
pub struct Pseudo {
    pub id: PseudoId,
    pub kind: PseudoKind,
    /// Optional name for debugging (from source variable)
    pub name: Option<String>,
}

impl PartialEq for Pseudo {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.kind == other.kind
    }
}

impl Pseudo {
    pub fn undef(id: PseudoId) -> Self {
        Self {
            id,
            kind: PseudoKind::Undef,
            name: None,
        }
    }

    pub fn reg(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Reg(nr),
            name: None,
        }
    }

    /// Create an argument pseudo
    pub fn arg(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Arg(nr),
            name: None,
        }
    }

    pub fn phi(id: PseudoId, nr: u32) -> Self {
        Self {
            id,
            kind: PseudoKind::Phi(nr),
            name: None,
        }
    }

    pub fn sym(id: PseudoId, name: String) -> Self {
        Self {
            id,
            kind: PseudoKind::Sym(name.clone()),
            name: Some(name),
        }
    }

    /// Create a constant value pseudo
    pub fn val(id: PseudoId, value: i128) -> Self {
        Self {
            id,
            kind: PseudoKind::Val(value),
            name: None,
        }
    }

    /// Create a constant float pseudo
    pub fn fval(id: PseudoId, value: FloatVal) -> Self {
        Self {
            id,
            kind: PseudoKind::FVal(value),
            name: None,
        }
    }

    /// With a name for debugging
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

impl fmt::Display for Pseudo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            PseudoKind::Void => write!(f, "VOID"),
            PseudoKind::Undef => write!(f, "UNDEF"),
            PseudoKind::Reg(nr) => {
                if let Some(name) = &self.name {
                    write!(f, "%r{}({})", nr, name)
                } else {
                    write!(f, "%r{}", nr)
                }
            }
            PseudoKind::Arg(nr) => write!(f, "%arg{}", nr),
            PseudoKind::Phi(nr) => {
                if let Some(name) = &self.name {
                    write!(f, "%phi{}({})", nr, name)
                } else {
                    write!(f, "%phi{}", nr)
                }
            }
            PseudoKind::Sym(name) => write!(f, "{}", name),
            PseudoKind::Val(v) => {
                if *v > 1000 || *v < -1000 {
                    write!(f, "${:#x}", v)
                } else {
                    write!(f, "${}", v)
                }
            }
            PseudoKind::FVal(v) => write!(f, "${}", v),
        }
    }
}

// BasicBlock ID

/// Unique ID for a basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BasicBlockId(pub u32);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".L{}", self.0)
    }
}

impl BasicBlockId {
    /// The assembler symbol that names this block of function `func_name`,
    /// unquoted: what `&&label` evaluates to, and what the backends define at
    /// the head of the block.
    ///
    /// One spelling for both, because a reference and a definition that
    /// disagree link against nothing -- and the inliner has to rebuild the
    /// name when it moves a block into another function.
    pub fn label_symbol(self, func_name: &str) -> String {
        format!(".L{}_{}", func_name, self.0)
    }
}

// Inline Assembly Support

/// Constraint information for an inline asm operand
#[derive(Debug, Clone)]
pub struct AsmConstraint {
    /// The pseudo (register or value) for this operand
    pub pseudo: PseudoId,
    /// Optional symbolic name for the operand (e.g., [result])
    pub name: Option<String>,
    /// The output operand this input shares its operand with: an explicit
    /// matching constraint (`"0"`), or the hidden input a `"+"` output
    /// implies.
    pub matching_output: Option<usize>,
    /// The constraint string as written (e.g. "r", "=r", "+m", "=&r"). Only
    /// diagnostics and dumps read it; what it allows is [`Self::class`].
    pub constraint: String,
    /// What the constraint allows, classified once for the target. Every
    /// question about the operand's letters is answered from here.
    pub class: AsmOperandClass,
    /// Size of the operand in bits (8, 16, 32, 64), derived from the C type
    pub size: u32,
    /// For a memory operand whose `pseudo` is a `Sym` -- the object itself,
    /// not an address held in a value -- the byte offset into that object.
    /// Zero otherwise.
    ///
    /// A named object at a constant offset needs no register: the backend
    /// addresses it where it lives (`-N(%rbp)`, `[x29, #N]`, `sym(%rip)`), as
    /// gcc does. Passed as an address value instead, it competed for a
    /// register like any other operand and, once a statement had more
    /// operands than registers, was spilled -- and the slot holding the
    /// address was then substituted as if it were the object.
    pub offset: i64,
}

impl AsmConstraint {
    /// An operand of `constraint` on `arch`, `size` bits wide, with no name,
    /// matching output or object offset.
    #[cfg(test)]
    pub fn new(pseudo: PseudoId, constraint: &str, arch: crate::target::Arch, size: u32) -> Self {
        Self {
            pseudo,
            name: None,
            matching_output: None,
            constraint: constraint.to_string(),
            class: AsmOperandClass::parse(constraint, arch).expect("a modelled constraint"),
            size,
            offset: 0,
        }
    }

    /// True when the assembler receives this operand as a memory reference
    /// rather than a value in a register.
    ///
    /// This matters for liveness in a way that is easy to miss: a memory
    /// *output* still **reads** its pseudo, because the pseudo holds the
    /// address the assembly writes through. Anything that computes uses must
    /// call this, or DCE deletes what materialized the address.
    ///
    /// A constraint may offer several alternatives (`"rm"`); it is only a
    /// memory operand if no register/immediate alternative is available.
    pub fn is_memory(&self) -> bool {
        self.class.is_memory_only()
    }

    /// True for an early-clobber output (`"=&r"`, `"+&r"`, `"&=r"`): the
    /// template writes it before it has read every input, so it may not share
    /// a register with any input, even one whose value dies at the asm.
    pub fn is_early_clobber(&self) -> bool {
        self.class.early_clobber
    }

    /// True when the operand may be given a register: a register class
    /// (`r`, a named register letter, `g`, ...) or a matching constraint,
    /// rather than only memory or only an immediate (`i`, `n`).
    pub fn wants_register(&self) -> bool {
        self.class.reg.is_some() || self.class.tied.is_some()
    }

    /// True for the input a `"+"` output implies: it carries the output's
    /// constraint (`"+r"`) rather than a matching digit (`"0"`).
    ///
    /// gcc numbers operands as outputs, then the inputs the source wrote --
    /// an explicit `"0"` among them -- and only then these hidden inputs, in
    /// output order; `asm goto` labels come after all of them. So a hidden
    /// input takes the *last* operand numbers, never one between the explicit
    /// ones.
    pub fn is_hidden_readwrite_input(&self) -> bool {
        self.matching_output.is_some() && self.class.access == AsmAccess::ReadWrite
    }
}

/// Data for an inline assembly instruction
#[derive(Debug, Clone)]
pub struct AsmData {
    /// The assembly template string
    pub template: String,
    /// Output operands
    pub outputs: Vec<AsmConstraint>,
    /// Input operands
    pub inputs: Vec<AsmConstraint>,
    /// Clobber list: registers and special values ("memory", "cc")
    pub clobbers: Vec<String>,
    /// Goto labels for asm goto: BasicBlockIds the asm can jump to
    /// Referenced in template as %l0, %l1, ... or %l[name]
    pub goto_labels: Vec<(BasicBlockId, String)>,
}

// Instruction

/// Every `PseudoId` slot of an [`Instruction`] that [`Instruction::mentioned`]
/// reads, as an iterator of references: target, sources, indirect call
/// target, phi operands, then asm outputs and inputs. Expanded once over a
/// shared instruction and once (with `mut`) over a unique one, so the walk
/// that reads the slots and the walk that rewrites them are one list.
///
/// The order is the inliner's: it allocates a caller pseudo for each callee
/// pseudo in the order it first meets them.
macro_rules! mention_slots {
    ($insn:expr $(, $m:tt)?) => {{
        let Instruction {
            target,
            src,
            phi_list,
            extra,
            ..
        } = $insn;
        let (indirect, asm) = match extra {
            Some(e) => {
                let InsnExtra {
                    indirect_target,
                    asm_data,
                    ..
                } = &$($m)? **e;
                (Some(indirect_target), Some(asm_data))
            }
            None => (None, None),
        };
        target
            .into_iter()
            .chain(src)
            .chain(indirect.into_iter().flatten())
            .chain(phi_list.into_iter().map(|(_, p)| p))
            .chain(
                asm.into_iter()
                    .flatten()
                    .flat_map(|d| (&$($m)? d.outputs).into_iter().chain(&$($m)? d.inputs))
                    .map(|c| &$($m)? c.pseudo),
            )
    }};
}

/// An IR instruction
#[derive(Debug, Clone)]
pub struct Instruction {
    /// Opcode
    pub op: Opcode,
    /// Result pseudo (target)
    pub target: Option<PseudoId>,
    /// Source operands
    pub src: Vec<PseudoId>,
    /// Type of the result (interned TypeId)
    pub typ: Option<TypeId>,
    /// For branches: true target
    pub bb_true: Option<BasicBlockId>,
    /// For conditional branches: false target
    pub bb_false: Option<BasicBlockId>,
    /// For memory ops: offset, read by a backend through
    /// [`Instruction::displacement`]. For `FrameAddress`/`ReturnAddress`: the
    /// level, read through [`Instruction::frame_level`].
    pub offset: i64,
    /// For phi nodes: list of (bb, pseudo) pairs
    pub phi_list: Vec<(BasicBlockId, PseudoId)>,
    /// Bit size of the result -- for every opcode.
    pub size: u32,
    /// Bit size of the operands, for an opcode whose operands are of another
    /// type than its result ([`Opcode::reads_another_type`]); 0 otherwise.
    /// Read it through [`Instruction::operand_width`].
    pub src_size: u32,
    /// Source type for conversion operations (interned TypeId)
    pub src_typ: Option<TypeId>,
    /// Source position for debug info
    pub pos: Option<Position>,
    /// For `Load` and `Store`: the object being accessed is `volatile`, so the
    /// access itself is observable behaviour (C17 5.1.2.3p6) and no pass may
    /// delete, merge, move or fold it.
    ///
    /// The qualifier lives on the *access*, not on the variable, because for
    /// `volatile int *p` there is no variable to ask: `p` is an ordinary
    /// pointer and `*p` is the volatile object. `LocalVar::is_volatile` and
    /// `memloc::GlobalFacts::is_volatile` answer only for a named object, so
    /// DCE saw nothing to stop it and deleted every discarded `volatile` read
    /// from `-O1` up. Ask through [`Instruction::is_volatile_access`].
    ///
    /// Set for every access the linearizer emits, from the type it is
    /// accessing, in `Linearizer::mark_volatile_access`.
    pub is_volatile: bool,
    /// The fields only calls, switches, `asm` and atomics use, in one box
    /// that every other instruction leaves empty. Read them through
    /// [`Instruction::extra`] and set them through [`Instruction::extra_mut`].
    pub extra: Option<Box<InsnExtra>>,
}

/// The fields of an [`Instruction`] that only a few opcodes use.
///
/// Kept out of line because most instructions are loads, stores, copies and
/// arithmetic, which carry none of them: inline, they made every instruction
/// several times the size of the ones that need them.
#[derive(Debug, Clone, Default)]
pub struct InsnExtra {
    /// For calls: function name or pseudo
    pub func_name: Option<String>,
    /// For switch: case range to target block mapping, as `(lo, hi, block)`.
    ///
    /// An ordinary `case v:` is the degenerate range `(v, v, block)`. Ranges
    /// are held rather than expanded because the GNU `case lo ... hi:` form
    /// admits `case 0 ... 1000000:`, and each entry costs a basic block and a
    /// compare in both backends.
    pub switch_cases: Vec<(i64, i64, BasicBlockId)>,
    /// For switch: default block (if no case matches)
    pub switch_default: Option<BasicBlockId>,
    /// For calls: argument types (parallel to src for Call instructions, interned TypeIds)
    pub arg_types: Vec<TypeId>,
    /// For variadic calls: index where variadic arguments start (0-based)
    /// All arguments at this index and beyond are variadic (should be passed on stack)
    pub variadic_arg_start: Option<usize>,
    /// For calls: the argument list ended with `__builtin_va_arg_pack()`.
    ///
    /// The enclosing function's own variadic arguments belong here, and are
    /// only known once it is inlined, so the inliner appends them and clears
    /// this. A call still carrying it after inlining cannot be emitted, and is
    /// diagnosed by `opt::check_forwarding_resolved`.
    pub ends_with_va_arg_pack: bool,
    /// For calls: true if the called function is noreturn (never returns).
    /// Code after a noreturn call is unreachable.
    pub is_noreturn_call: bool,
    /// For direct calls: whether `func_name` may be a definition in this
    /// module or is only ever the external library function. See
    /// [`Instruction::local_callee`], which is how a pass should ask.
    pub callee_binding: crate::parse::ast::CalleeBinding,
    /// For calls: the library function called, when the program's name for
    /// it still means that function (see [`crate::parse::ast::LibFn`]).
    /// What `ir::libcall_fold` folds, and nothing else reads.
    pub known: Option<crate::parse::ast::LibFn>,
    /// For indirect calls: pseudo containing the function pointer address.
    /// When this is Some, the call is indirect (call through function pointer).
    pub indirect_target: Option<PseudoId>,
    /// For inline assembly: the asm data (template, operands, clobbers)
    pub asm_data: Option<Box<AsmData>>,
    /// For calls: rich ABI classification for arguments and return value.
    /// Derive sret/two-reg-return status via `returns_via_sret()` / `returns_two_regs()`.
    pub abi_info: Option<Box<CallAbiInfo>>,
    /// For atomic operations: memory ordering constraint
    pub memory_order: MemoryOrder,
    /// For `Fence`: whether it orders against other threads or only a
    /// signal handler on this one.
    pub fence_scope: FenceScope,
    /// For `LifetimeEnd`: the local whose lifetime ends.
    pub lifetime_of: Option<PseudoId>,
    /// For `Simd(Shuffle)`: the lanes it picks.
    pub shuffle: Option<ShuffleIndices>,
    /// For `Setjmp` and `Longjmp`: the library's, or gcc's builtin pair.
    /// Read it through [`Instruction::jmp_kind`].
    pub jmp_kind: crate::parse::ast::JmpKind,
}

/// What an instruction with no extra fields answers: every one empty.
static NO_EXTRA: InsnExtra = InsnExtra {
    func_name: None,
    switch_cases: Vec::new(),
    switch_default: None,
    arg_types: Vec::new(),
    variadic_arg_start: None,
    ends_with_va_arg_pack: false,
    is_noreturn_call: false,
    callee_binding: crate::parse::ast::CalleeBinding::Declared,
    known: None,
    indirect_target: None,
    asm_data: None,
    abi_info: None,
    memory_order: MemoryOrder::Relaxed,
    fence_scope: FenceScope::Thread,
    lifetime_of: None,
    shuffle: None,
    jmp_kind: crate::parse::ast::JmpKind::Library,
};

impl Default for Instruction {
    fn default() -> Self {
        Self {
            op: Opcode::Nop,
            target: None,
            src: Vec::new(),
            typ: None,
            bb_true: None,
            bb_false: None,
            offset: 0,
            phi_list: Vec::new(),
            size: 0,
            src_size: 0,
            src_typ: None,
            pos: None,
            is_volatile: false,
            extra: None,
        }
    }
}

impl Instruction {
    /// The fields only a few opcodes use; all empty unless set.
    pub fn extra(&self) -> &InsnExtra {
        self.extra.as_deref().unwrap_or(&NO_EXTRA)
    }

    /// The fields only a few opcodes use, for setting.
    pub fn extra_mut(&mut self) -> &mut InsnExtra {
        self.extra.get_or_insert_with(Default::default)
    }

    /// The lanes a `Simd(Shuffle)` picks.
    pub fn shuffle_indices(&self) -> &ShuffleIndices {
        self.extra()
            .shuffle
            .as_ref()
            .expect("a shuffle carries its indices")
    }

    pub fn new(op: Opcode) -> Self {
        Self {
            op,
            ..Default::default()
        }
    }

    /// The function in this module a direct call may run, by name.
    ///
    /// `None` for anything but a direct call, and for a call to a library
    /// function spelled `__builtin_X`: that one reaches the external `X`
    /// however this module defines `X`, so it is neither a call to an inline
    /// definition to splice in nor a recursive call when made from `X`'s own
    /// body. Inlining and recursion detection ask this rather than reading
    /// `func_name`, which names the symbol either way.
    pub fn local_callee(&self) -> Option<&str> {
        match (self.op, self.extra().callee_binding) {
            (Opcode::Call, crate::parse::ast::CalleeBinding::Declared) => {
                self.extra().func_name.as_deref()
            }
            _ => None,
        }
    }

    /// Set the target
    pub fn with_target(mut self, target: PseudoId) -> Self {
        self.target = Some(target);
        self
    }

    /// Add a source operand
    pub fn with_src(mut self, src: PseudoId) -> Self {
        self.src.push(src);
        self
    }

    /// Set src1 and src2 (for binary ops)
    pub fn with_src2(mut self, src1: PseudoId, src2: PseudoId) -> Self {
        self.src = vec![src1, src2];
        self
    }

    /// Set src1, src2, src3 (for ternary ops like select)
    pub fn with_src3(mut self, src1: PseudoId, src2: PseudoId, src3: PseudoId) -> Self {
        self.src = vec![src1, src2, src3];
        self
    }

    /// Mark this `Load` or `Store` as an access to a `volatile` object.
    pub fn with_volatile(mut self, is_volatile: bool) -> Self {
        debug_assert!(
            !is_volatile || matches!(self.op, Opcode::Load | Opcode::Store),
            "only a Load or a Store carries the volatile marker"
        );
        self.is_volatile = is_volatile;
        self
    }

    /// Is this an access to a `volatile` object?
    ///
    /// Reading or writing one is observable behaviour (C17 5.1.2.3p6), so an
    /// access that answers `true` survives every optimization level: no pass
    /// may delete it, fold it to a constant, merge it with another access, or
    /// promote the object it reaches out of memory. This is the question to
    /// ask; `is_volatile` is only where the answer is stored, and is true of
    /// nothing but a `Load` or a `Store`.
    pub fn is_volatile_access(&self) -> bool {
        self.is_volatile && matches!(self.op, Opcode::Load | Opcode::Store)
    }

    /// Set the type (caller should also call with_size if needed)
    pub fn with_type(mut self, typ: TypeId) -> Self {
        self.typ = Some(typ);
        self
    }

    /// Set type and size together (convenience for callers with TypeTable access)
    pub fn with_type_and_size(mut self, typ: TypeId, size: u32) -> Self {
        self.typ = Some(typ);
        self.size = size;
        self
    }

    /// Set the true branch target
    pub fn with_bb_true(mut self, bb: BasicBlockId) -> Self {
        self.bb_true = Some(bb);
        self
    }

    /// Set the false branch target
    pub fn with_bb_false(mut self, bb: BasicBlockId) -> Self {
        self.bb_false = Some(bb);
        self
    }

    /// Set memory offset
    pub fn with_offset(mut self, offset: i64) -> Self {
        self.offset = offset;
        self
    }

    /// Set function name for calls
    pub fn with_func(mut self, name: impl Into<String>) -> Self {
        self.extra_mut().func_name = Some(name.into());
        self
    }

    /// The C library function an opcode the backends lower to a call
    /// (`Memcpy`, `Memset`, `Memmove`, `Setjmp`, `Longjmp`) calls, or a libm
    /// opcode (`Sqrt`) calls on a target without the instruction, by its
    /// assembler name.
    ///
    /// The linearizer resolved it through the program's own declarations
    /// (`Linearizer::library_function_name`), so an asm-label rename of
    /// `memcpy` reaches `__builtin_memcpy` and a structure copy alike. A
    /// backend names the callee through here, never with a literal.
    pub fn library_callee(&self) -> &str {
        match &self.extra().func_name {
            Some(name) => name,
            None => panic!("{:?} was built without its library callee", self.op),
        }
    }

    /// Which `setjmp`/`longjmp` a `Setjmp` or `Longjmp` is: the library's,
    /// a call naming [`Self::library_callee`], or gcc's builtin pair, which
    /// names none and is generated inline.
    pub fn jmp_kind(&self) -> crate::parse::ast::JmpKind {
        self.extra().jmp_kind
    }

    /// Is this gcc's `__builtin_setjmp`? Control resumes just after it with
    /// no register intact but the frame and stack pointers, so the register
    /// allocator keeps nothing in a register across it, and the function
    /// saves every callee-saved register.
    pub fn is_builtin_setjmp(&self) -> bool {
        self.op == Opcode::Setjmp && self.jmp_kind() == crate::parse::ast::JmpKind::Builtin
    }

    /// Set bit size
    pub fn with_size(mut self, size: u32) -> Self {
        self.size = size;
        self
    }

    /// Set source position for debug info
    pub fn with_pos(mut self, pos: Position) -> Self {
        self.pos = Some(pos);
        self
    }

    /// Set memory ordering for atomic operations
    pub fn with_memory_order(mut self, order: MemoryOrder) -> Self {
        self.extra_mut().memory_order = order;
        self
    }

    /// The order a `Fence` asks the hardware for: its own for a thread
    /// fence, and `None` for a signal fence, which needs no instruction.
    /// What each back end maps to its barrier.
    pub fn hardware_fence_order(&self) -> Option<MemoryOrder> {
        let extra = self.extra();
        match extra.fence_scope {
            FenceScope::Thread => Some(extra.memory_order),
            FenceScope::Signal => None,
        }
    }

    /// Returns true if this instruction acts as a memory-reordering
    /// barrier — no IR pass may move a `Load`/`Store` (or any other
    /// memory-touching op) across this instruction in either direction.
    ///
    /// Sources of barrier semantics:
    /// - `Opcode::Asm` with `"memory"` in its clobber list (the
    ///   "compiler memory barrier" idiom used by `pause`/`yield`
    ///   spin loops, ticket-lock acquire, `__sync_synchronize`-style
    ///   fences, etc.).
    /// - `Opcode::Fence` — explicit C11 `atomic_thread_fence`, and
    ///   `atomic_signal_fence` exactly as much: a signal handler sees memory
    ///   only as the compiler left it, though no instruction is emitted.
    /// - `Opcode::Atomic*` — every atomic memory op (including
    ///   `Relaxed`-ordered ones — see note below).
    /// - `Opcode::Call` — a callee may read or write any memory it can
    ///   reach, and this predicate does not ask what that is.
    /// - `Opcode::Setjmp` / `Opcode::Longjmp` — non-local control flow
    ///   makes register/memory state observable at any saved jmp_buf.
    ///
    /// **Contract**: an IR pass MUST NOT move a memory operation across an
    /// instruction for which this returns `true`. This is the load-bearing
    /// invariant that lets inline `asm("..." ::: "memory")` actually mean
    /// something. No pass moves a memory operation: `loadfwd` and `dse`
    /// remove loads and stores rather than move them, and decide what a
    /// call can reach from `escape.rs` and the callee's effects; every other
    /// barrier opcode they treat as touching all memory.
    ///
    /// **This predicate answers *ordering*, not *extent*, and it is not the
    /// list of instructions that touch memory.** `Store`, `Memset`,
    /// `Memcpy`, `Memmove`, the `Va*` family, `Alloca` and
    /// `StackSave`/`StackRestore` all access memory and are deliberately
    /// absent: a `memcpy` is an access, not a fence, and conflating the two
    /// would over-restrict a scheduler later. The question "which addresses
    /// does this reach?" is [`Opcode::may_access_memory`] at the opcode
    /// level. A pass that moves memory must satisfy both.
    ///
    /// Note on relaxed atomics: a `MemoryOrder::Relaxed` atomic op
    /// has no inter-thread ordering guarantee, but the atomic access
    /// itself is still a side-effecting memory op that a reordering
    /// pass cannot freely move past unrelated loads/stores. Future
    /// finer-grained one-sided predicates
    /// (`is_acquire_barrier` / `is_release_barrier`) can refine this
    /// when a pass needs the distinction.
    pub fn is_memory_barrier(&self) -> bool {
        match self.op {
            Opcode::Asm => self
                .extra()
                .asm_data
                .as_ref()
                .is_some_and(|d| d.clobbers.iter().any(|c| c == "memory")),
            Opcode::Fence | Opcode::Call | Opcode::Setjmp | Opcode::Longjmp => true,
            op => op.is_atomic(),
        }
    }

    /// Does this instruction mention `id` in any operand position at all?
    ///
    /// **The canonical enumeration of every place a `PseudoId` can be written
    /// down**, and deliberately wider than [`Self::uses`]: it counts the
    /// target and a `PhiSource`'s back-pointer, which are definitions rather
    /// than operands. That is what an analysis asking "could this pseudo have
    /// leaked?" needs, so a symbol reaching an opcode the analysis does not
    /// model reads as an escape rather than as nothing.
    ///
    /// If a field that can hold a `PseudoId` is ever added to `Instruction`,
    /// it must be added to `mention_slots`, which this walks, or an address
    /// escapes invisibly. The one field left out is `lifetime_of`: a
    /// lifetime marker is out of band, and is not a mention of its local.
    pub fn mentions(&self, id: PseudoId) -> bool {
        self.mentioned().any(|p| p == id)
    }

    /// Every pseudo this instruction names, in any role, possibly repeated --
    /// the enumeration [`Instruction::mentions`] asks about one pseudo at a
    /// time. For a pass that needs the answer for every symbol at once, which
    /// asking `mentions` per symbol per instruction makes quadratic.
    pub fn mentioned(&self) -> impl Iterator<Item = PseudoId> + '_ {
        mention_slots!(self).copied()
    }

    /// Rewrite every pseudo this instruction names: each slot
    /// [`Self::mentioned`] reads, in the same order, and then the local a
    /// `LifetimeEnd` names.
    ///
    /// That last one is the only `PseudoId` field `mentioned` leaves out (see
    /// [`Self::mentions`]), but renaming the local renames it too. Built on
    /// the same slot walk as `mentioned`, so the two cannot drift apart.
    pub fn for_each_pseudo_mut(&mut self, mut f: impl FnMut(&mut PseudoId)) {
        mention_slots!(self, mut).for_each(&mut f);
        if let Some(extra) = self.extra.as_deref_mut() {
            extra.lifetime_of.iter_mut().for_each(f);
        }
    }

    /// Every pseudo this instruction reads.
    ///
    /// The canonical enumeration, because uses are not all in `src`: a `Phi`
    /// reads the pseudos named in `phi_list`, an indirect call reads
    /// `indirect_target`, and inline assembly reads its `inputs` -- plus a
    /// *memory* output, whose pseudo is the address the assembly writes
    /// through rather than the value written. Omitting those let DCE delete
    /// the address computation, so every `"=m"` operand became a store
    /// through a garbage register.
    ///
    /// The exception that bites: a `PhiSource`'s own `phi_list` is a
    /// back-pointer to the `Phi` it feeds, not an operand. Counting it as a
    /// use makes the value look live to DCE and makes a def-use graph report
    /// an edge that runs the wrong way.
    pub fn uses(&self) -> Vec<PseudoId> {
        let mut uses = Vec::with_capacity(DEFAULT_USE_CAPACITY);

        uses.extend(self.src.iter().copied());

        if self.op != Opcode::PhiSource {
            for (_, pseudo) in &self.phi_list {
                uses.push(*pseudo);
            }
        }

        if let Some(indirect) = self.extra().indirect_target {
            uses.push(indirect);
        }

        if let Some(ref asm_data) = self.extra().asm_data {
            for input in &asm_data.inputs {
                uses.push(input.pseudo);
            }
            for output in &asm_data.outputs {
                if output.is_memory() {
                    uses.push(output.pseudo);
                }
            }
        }

        uses
    }

    /// Create a return instruction
    pub fn ret(src: Option<PseudoId>) -> Self {
        let mut insn = Self::new(Opcode::Ret);
        if let Some(s) = src {
            insn.src.push(s);
        }
        insn
    }

    /// Create a return instruction with type
    pub fn ret_typed(src: Option<PseudoId>, typ: TypeId, size: u32) -> Self {
        Self::ret(src).with_type_and_size(typ, size)
    }

    /// Create an unconditional branch
    pub fn br(target: BasicBlockId) -> Self {
        Self::new(Opcode::Br).with_bb_true(target)
    }

    /// The end of `local`'s lifetime: see [`Opcode::LifetimeEnd`].
    pub fn lifetime_end(local: PseudoId) -> Self {
        let mut insn = Self::new(Opcode::LifetimeEnd);
        insn.extra_mut().lifetime_of = Some(local);
        insn
    }

    /// Create a conditional branch
    pub fn cbr(cond: PseudoId, bb_true: BasicBlockId, bb_false: BasicBlockId) -> Self {
        Self::new(Opcode::Cbr)
            .with_src(cond)
            .with_bb_true(bb_true)
            .with_bb_false(bb_false)
    }

    /// GNU computed goto: branch to the address held in `target`.
    pub fn indirect_br(target: PseudoId) -> Self {
        Self::new(Opcode::IndirectBr).with_src(target)
    }

    /// Create a switch instruction
    pub fn switch_insn(
        value: PseudoId,
        cases: Vec<(i64, i64, BasicBlockId)>,
        default: Option<BasicBlockId>,
        size: u32,
    ) -> Self {
        Self {
            op: Opcode::Switch,
            src: vec![value],
            size,
            extra: Some(Box::new(InsnExtra {
                switch_cases: cases,
                switch_default: default,
                ..Default::default()
            })),
            ..Default::default()
        }
    }

    /// Create a binary operation
    pub fn binop(
        op: Opcode,
        target: PseudoId,
        src1: PseudoId,
        src2: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> Self {
        // Not an assertion only debug builds make: every gate runs release.
        assert!(
            !op.is_comparison(),
            "{op:?} is a comparison: build it with Instruction::compare"
        );
        Self::new(op)
            .with_target(target)
            .with_src2(src1, src2)
            .with_type_and_size(typ, size)
    }

    /// A comparison `op` of `lhs` and `rhs`, which are of type `operand.0` at
    /// `operand.1` bits, producing a 0-or-1 of type `result.0` at `result.1`
    /// bits -- an `int` for C's operators, a `_Bool` for a conversion to one.
    ///
    /// The one constructor for a comparison, so that each records its
    /// operands where every other opcode that reads another type does
    /// (`src_typ`/`src_size`) and its result where every opcode does
    /// (`typ`/`size`). They used to put the *operand* type in `typ`, the only
    /// opcodes to do so, and two folds that copied a comparison's `typ` onto
    /// the constant replacing it were miscompiles.
    pub fn compare(
        op: Opcode,
        target: PseudoId,
        (lhs, rhs): (PseudoId, PseudoId),
        operand: (TypeId, u32),
        result: (TypeId, u32),
    ) -> Self {
        assert!(op.is_comparison(), "{op:?} is not a comparison");
        let mut insn = Self::new(op)
            .with_target(target)
            .with_src2(lhs, rhs)
            .with_type_and_size(result.0, result.1);
        insn.src_typ = Some(operand.0);
        insn.src_size = operand.1;
        insn
    }

    /// `op` of `a` and `b` into `target`, as a comparison producing an `int`
    /// when `op` is one and as an ordinary binary operation of `typ` at
    /// `size` bits otherwise. For tests whose helpers take the opcode as a
    /// parameter.
    #[cfg(test)]
    pub fn test_binary(
        op: Opcode,
        target: PseudoId,
        (a, b): (PseudoId, PseudoId),
        typ: TypeId,
        size: u32,
    ) -> Self {
        if op.is_comparison() {
            let int = crate::types::TypeTable::new(&crate::target::Target::host()).int_id;
            Self::compare(op, target, (a, b), (typ, size), (int, 32))
        } else {
            Self::binop(op, target, a, b, typ, size)
        }
    }

    /// The type of this instruction's operands: `src_typ` for an opcode that
    /// reads another type than it produces ([`Opcode::reads_another_type`]),
    /// `typ` for every other.
    pub fn operand_type(&self) -> Option<TypeId> {
        if self.op.reads_another_type() {
            self.src_typ
        } else {
            self.typ
        }
    }

    /// Whether running this instruction can raise a floating-point
    /// exception ([`FpRaise`]): its opcode's answer, with a conversion's
    /// decided by the two types it converts between. A conversion that does
    /// not record its source type is assumed to raise.
    pub fn may_raise_fp_exception(&self, types: &TypeTable) -> bool {
        match self.op.fp_raise() {
            FpRaise::Never => false,
            FpRaise::May => true,
            FpRaise::Conversion => match (self.src_typ, self.typ) {
                (Some(from), Some(to)) => conversion_raises_fp(types, from, to),
                _ => true,
            },
        }
    }

    /// The width of this instruction's operands; see [`Self::operand_type`].
    pub fn operand_width(&self) -> u32 {
        if self.op.reads_another_type() {
            self.src_size
        } else {
            self.size
        }
    }

    /// Create a unary operation
    pub fn unop(op: Opcode, target: PseudoId, src: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(op)
            .with_target(target)
            .with_src(src)
            .with_type_and_size(typ, size)
    }

    /// `FrameAddress` or `ReturnAddress` for `level` frames up. The level is a
    /// constant the parser has already evaluated, so it travels as an
    /// immediate rather than a pseudo: the backend walks that many frame
    /// records, which it cannot do for a run-time value.
    pub fn frame_walk(op: Opcode, target: PseudoId, level: u32, void_ptr: TypeId) -> Self {
        debug_assert!(matches!(op, Opcode::FrameAddress | Opcode::ReturnAddress));
        Self::new(op)
            .with_target(target)
            .with_offset(i64::from(level))
            .with_type_and_size(void_ptr, 64)
    }

    /// The level of a `FrameAddress`/`ReturnAddress` built by
    /// [`Instruction::frame_walk`].
    pub fn frame_level(&self) -> u32 {
        debug_assert!(matches!(
            self.op,
            Opcode::FrameAddress | Opcode::ReturnAddress
        ));
        self.offset as u32
    }

    /// A memory access's offset as the machine displacement it becomes.
    ///
    /// Always in range: `Linearizer::emit` folds any offset past `i32` into
    /// the address before the instruction enters the IR, and no pass rewrites
    /// an offset afterwards (`validate.rs` I6). The backends used to narrow
    /// with `as i32`, which wrapped a member more than 2 GiB into a struct to
    /// a displacement gigabytes away.
    pub fn displacement(&self) -> i32 {
        debug_assert!(self.op.addresses_memory());
        i32::try_from(self.offset)
            .expect("a memory access offset past i32 reached a backend; Linearizer::emit folds it")
    }

    pub fn load(target: PseudoId, addr: PseudoId, offset: i64, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Load)
            .with_target(target)
            .with_src(addr)
            .with_offset(offset)
            .with_type_and_size(typ, size)
    }

    pub fn store(value: PseudoId, addr: PseudoId, offset: i64, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Store)
            .with_src(addr)
            .with_src(value)
            .with_offset(offset)
            .with_type_and_size(typ, size)
    }

    pub fn call(
        target: Option<PseudoId>,
        func: &str,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        ret_size: u32,
    ) -> Self {
        let mut insn = Self::new(Opcode::Call)
            .with_func(func)
            .with_type_and_size(ret_type, ret_size);
        if let Some(t) = target {
            insn.target = Some(t);
        }
        insn.src = args;
        insn.extra_mut().arg_types = arg_types;
        insn
    }

    /// Create a call instruction with ABI classification.
    ///
    /// This is the canonical way for IR passes to synthesize call instructions
    /// (e.g., runtime library calls). It classifies parameters and return value
    /// using the given calling convention, attaches `CallAbiInfo`, and returns
    /// a ready-to-emit instruction.
    #[allow(clippy::too_many_arguments)]
    pub fn call_with_abi(
        target: Option<PseudoId>,
        func_name: &str,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        conv: CallingConv,
        types: &TypeTable,
        target_info: &Target,
    ) -> Self {
        let ret_size = types.size_bits(ret_type);
        let abi = get_abi_for_conv(conv, target_info);
        let param_classes: Vec<_> = arg_types
            .iter()
            .map(|&t| abi.classify_param(t, types))
            .collect();
        let ret_class = abi.classify_return(ret_type, types);
        let call_abi_info = Box::new(CallAbiInfo::with_conv(param_classes, ret_class, conv));

        let mut insn = Self::call(target, func_name, args, arg_types, ret_type, ret_size);
        insn.extra_mut().abi_info = Some(call_abi_info);
        insn
    }

    /// Create an indirect call instruction (call through function pointer)
    pub fn call_indirect(
        target: Option<PseudoId>,
        func_addr: PseudoId,
        args: Vec<PseudoId>,
        arg_types: Vec<TypeId>,
        ret_type: TypeId,
        ret_size: u32,
    ) -> Self {
        let mut insn = Self::call(target, "<indirect>", args, arg_types, ret_type, ret_size);
        insn.extra_mut().indirect_target = Some(func_addr);
        insn
    }

    /// Create a symbol address instruction (get address of a symbol like string literals)
    pub fn sym_addr(target: PseudoId, sym: PseudoId, typ: TypeId) -> Self {
        Self::new(Opcode::SymAddr)
            .with_target(target)
            .with_src(sym)
            .with_type_and_size(typ, 64) // Pointers are always 64-bit
    }

    /// Create a thread-local address instruction. See [`Opcode::TlsAddr`].
    pub fn tls_addr(target: PseudoId, sym: PseudoId, typ: TypeId) -> Self {
        Self::new(Opcode::TlsAddr)
            .with_target(target)
            .with_src(sym)
            .with_type_and_size(typ, 64)
    }

    pub fn phi(target: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::Phi)
            .with_target(target)
            .with_type_and_size(typ, size)
    }

    /// The `SetVal` that defines the constant pseudo `target` at `typ` and
    /// `size` bits. The value lives in `target` (`PseudoKind::Val`/`FVal`),
    /// never in the instruction.
    pub fn set_val(target: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::SetVal)
            .with_target(target)
            .with_type_and_size(typ, size)
    }

    /// Create a phi source instruction (placed in predecessor block).
    /// Back-pointer to owning phi is stored in phi_list by the caller.
    pub fn phi_source(target: PseudoId, src: PseudoId, typ: TypeId, size: u32) -> Self {
        Self::new(Opcode::PhiSource)
            .with_target(target)
            .with_src(src)
            .with_type_and_size(typ, size)
    }

    /// The phi a `PhiSource` feeds: the phi's block and its target, read from
    /// the back-pointer in `phi_list`. `None` for any other opcode, whose
    /// `phi_list` (a phi's incoming pairs, or nothing) is no back-pointer.
    pub fn phi_source_dest(&self) -> Option<(BasicBlockId, PseudoId)> {
        if self.op == Opcode::PhiSource {
            self.phi_list.first().copied()
        } else {
            None
        }
    }

    /// Create a select (ternary) instruction for pure expressions
    /// Enables cmov/csel codegen instead of branches
    pub fn select(
        target: PseudoId,
        cond: PseudoId,
        if_true: PseudoId,
        if_false: PseudoId,
        typ: TypeId,
        size: u32,
    ) -> Self {
        Self::new(Opcode::Select)
            .with_target(target)
            .with_src3(cond, if_true, if_false)
            .with_type_and_size(typ, size)
    }

    /// Create an inline assembly instruction
    pub fn asm(data: AsmData) -> Self {
        Self {
            op: Opcode::Asm,
            extra: Some(Box::new(InsnExtra {
                asm_data: Some(Box::new(data)),
                ..Default::default()
            })),
            ..Default::default()
        }
    }

    /// The ABI classification of this call's or return's value, when the
    /// instruction carries one.
    pub fn ret_class(&self) -> Option<&ArgClass> {
        self.extra().abi_info.as_ref().map(|ai| &ai.ret)
    }

    /// Check if this call/return uses a hidden sret pointer for the return value.
    pub fn returns_via_sret(&self) -> bool {
        matches!(self.ret_class(), Some(ArgClass::Indirect { .. }))
    }

    /// True when this `Ret` hands its value back in st(0).
    ///
    /// The source is then the value's *address*, not the value: an x87 return
    /// is loaded onto the FPU stack from memory, since nothing else can hold
    /// an 80-bit value.
    pub fn returns_via_x87(&self) -> bool {
        matches!(self.ret_class(), Some(ArgClass::X87 { .. }))
    }

    /// Check if this call/return uses two registers for the return value.
    pub fn returns_two_regs(&self) -> bool {
        matches!(self.ret_class(), Some(ArgClass::Direct { classes, .. }) if classes.len() == 2)
    }

    /// True when this `Ret` hands back an aggregate by *address*: its source
    /// is a pointer to the value's storage, not the value.
    ///
    /// Asked of the `Ret`'s own ABI classification, which is the only place
    /// the answer is recorded -- `Instruction::size` is the aggregate's width,
    /// so [`aggregate_ret_is_address`] can apply its own size bound without a
    /// `TypeTable`. Only [`crate::ir::Linearizer::emit_reg_aggregate_return`] ever
    /// puts `abi_info` on a `Ret`, and only for a struct or union, so no
    /// scalar reaches this.
    pub fn returns_aggregate_address(&self) -> bool {
        self.ret_class()
            .is_some_and(|ret| aggregate_ret_is_address(ret, self.size))
    }

    /// Turn this instruction into a `Nop` that holds nothing at all.
    ///
    /// Every field is reset, not only the operands. Clearing four of them left
    /// a killed branch naming its targets, a killed call its callee and ABI
    /// record, a killed `asm` its operands and labels -- and every pass that
    /// walks fields rather than opcodes then saw a block edge, a use or a
    /// memory access that was not there, so each had to learn to skip `Nop`
    /// or to rewrite in place rather than kill.
    pub fn kill(&mut self) {
        *self = Instruction::new(Opcode::Nop);
    }
}

/// The tables an IR entity needs in order to print what it actually holds.
///
/// A bare `Display` impl could not reach any of them, and the dump said so:
/// `return 42;` printed as `%0 = setval.32` with the 42 nowhere on the line,
/// because a `SetVal`'s constant lives in its *target pseudo* and the
/// instruction has only that pseudo's id. Types printed as `type#7` and
/// symbols as a bare `%3` for the same reason. `ir/README.md` documented the
/// operand form -- `%1 = setval.32 $20` -- that the compiler had never
/// produced.
///
/// So the printer takes the tables. Every IR type is printed through
/// `.display(...)` rather than `{}`, which is what makes the id resolvable.
#[derive(Clone, Copy)]
pub struct IrCtx<'a> {
    types: &'a TypeTable,
    func: Option<&'a Function>,
}

impl<'a> IrCtx<'a> {
    /// A pseudo as what it *is*, not as its index.
    ///
    /// Registers, arguments and phis keep their id spelling: the id is what
    /// makes a dump followable from definition to use. Constants and symbols
    /// have no useful id, and printing one hid the only information they carry.
    fn pseudo(&self, id: PseudoId) -> String {
        match self.func.and_then(|f| f.get_pseudo(id)) {
            Some(p) => match &p.kind {
                // Both halves: the id keeps the line linked to the definition,
                // the payload is the thing that was missing.
                PseudoKind::Val(v) => format!("{}(${})", id, v),
                PseudoKind::FVal(v) => format!("{}(${})", id, v),
                PseudoKind::Sym(name) => format!("{}(@{})", id, name),
                PseudoKind::Void => "VOID".to_string(),
                PseudoKind::Undef => "UNDEF".to_string(),
                _ => format!("{}", id),
            },
            None => format!("{}", id),
        }
    }

    /// The constant a `SetVal`'s target carries, if it has one.
    fn target_const(&self, id: PseudoId) -> Option<String> {
        match self.func.and_then(|f| f.get_pseudo(id)).map(|p| &p.kind) {
            Some(PseudoKind::Val(v)) => Some(format!("${}", v)),
            Some(PseudoKind::FVal(v)) => Some(format!("${}", v)),
            _ => None,
        }
    }

    fn typ(&self, id: TypeId) -> String {
        self.types.format_type(id, None)
    }
}

/// An `Instruction` paired with the tables that make it printable.
pub struct InstructionDisplay<'a> {
    insn: &'a Instruction,
    ctx: IrCtx<'a>,
}

impl Instruction {
    /// Print this instruction against `ctx`'s tables.
    pub fn display<'a>(&'a self, ctx: IrCtx<'a>) -> InstructionDisplay<'a> {
        InstructionDisplay { insn: self, ctx }
    }
}

impl fmt::Display for InstructionDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (ctx, this) = (self.ctx, self.insn);
        // Format: target = op src1, src2
        if let Some(target) = &self.insn.target {
            write!(f, "{} = ", target)?;
        }

        write!(f, "{}", self.insn.op.name())?;

        // Size suffix (for conversions, show src_size→size)
        if this.src_size > 0 && this.src_size != this.size && this.op.reads_another_type() {
            write!(f, ".{}to{}", this.src_size, this.size)?;
        } else if this.size > 0 {
            write!(f, ".{}", this.size)?;
        }

        // Operands depend on opcode
        match this.op {
            Opcode::Br => {
                if let Some(bb) = &this.bb_true {
                    write!(f, " {}", bb)?;
                }
            }
            Opcode::Cbr => {
                if let Some(cond) = this.src.first() {
                    write!(f, " {}", ctx.pseudo(*cond))?;
                }
                if let Some(bb) = &this.bb_true {
                    write!(f, ", {}", bb)?;
                }
                if let Some(bb) = &this.bb_false {
                    write!(f, ", {}", bb)?;
                }
            }
            // The constant is in the *target*, which is the whole reason this
            // printer takes the pseudo table.
            Opcode::SetVal => {
                if let Some(v) = this.target.and_then(|t| ctx.target_const(t)) {
                    write!(f, " {}", v)?;
                }
            }
            Opcode::Phi => {
                for (i, (bb, pseudo)) in this.phi_list.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {} ({})", ctx.pseudo(*pseudo), bb)?;
                }
            }
            Opcode::LifetimeEnd => {
                if let Some(local) = this.extra().lifetime_of {
                    write!(f, " {}", ctx.pseudo(local))?;
                }
            }
            Opcode::PhiSource => {
                if let Some(src) = this.src.first() {
                    write!(f, " {}", ctx.pseudo(*src))?;
                }
                if let Some((bb, pseudo)) = this.phi_source_dest() {
                    write!(f, " (-> {}:{})", bb, pseudo)?;
                }
            }
            Opcode::Call => {
                if let Some(func) = &this.extra().func_name {
                    write!(f, " {}", func)?;
                }
                write!(f, "(")?;
                for (i, arg) in this.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ctx.pseudo(*arg))?;
                }
                write!(f, ")")?;
            }
            Opcode::Switch => {
                if let Some(val) = this.src.first() {
                    write!(f, " {}", ctx.pseudo(*val))?;
                }
                for (lo, hi, bb) in &this.extra().switch_cases {
                    if lo == hi {
                        write!(f, ", {} => {}", lo, bb)?;
                    } else {
                        write!(f, ", {}..={} => {}", lo, hi, bb)?;
                    }
                }
                if let Some(default_bb) = &this.extra().switch_default {
                    write!(f, ", default => {}", default_bb)?;
                }
            }
            Opcode::Load | Opcode::Store => {
                for (i, src) in this.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ctx.pseudo(*src))?;
                }
                if this.offset != 0 {
                    write!(f, " + {}", this.offset)?;
                }
                if this.is_volatile {
                    write!(f, " volatile")?;
                }
            }
            _ => {
                for (i, src) in this.src.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ctx.pseudo(*src))?;
                }
            }
        }

        Ok(())
    }
}

// BasicBlock

/// A basic block - a sequence of instructions ending with a terminator
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Unique ID
    pub id: BasicBlockId,
    /// Instructions in this block
    pub insns: Vec<Instruction>,
    /// Predecessor blocks (CFG)
    pub parents: Vec<BasicBlockId>,
    /// Successor blocks (CFG)
    pub children: Vec<BasicBlockId>,
    /// Optional label name
    pub label: Option<String>,
    /// This block's address is taken by `&&label`, so it must be emitted even
    /// when no edge reaches it. Without this a function that stores a label
    /// address without branching on it -- legal GNU C -- lost the block to
    /// DCE, and the link failed on an undefined `.L` symbol.
    pub addr_taken: bool,
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self {
            id: BasicBlockId(0),
            insns: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
            label: None,
            addr_taken: false,
        }
    }
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> Self {
        Self {
            id,
            ..Default::default()
        }
    }

    /// Add an instruction to this block
    pub fn add_insn(&mut self, insn: Instruction) {
        self.insns.push(insn);
    }

    /// Insert an instruction before the terminator
    pub fn insert_before_terminator(&mut self, insn: Instruction) {
        if self.is_terminated() {
            let pos = self.insns.len() - 1;
            self.insns.insert(pos, insn);
        } else {
            self.insns.push(insn);
        }
    }

    pub fn is_terminated(&self) -> bool {
        self.insns
            .last()
            .map(|i| i.op.is_terminator())
            .unwrap_or(false)
    }
}

/// A `BasicBlock` paired with the tables that make its instructions printable.
pub struct BasicBlockDisplay<'a> {
    block: &'a BasicBlock,
    ctx: IrCtx<'a>,
}

impl BasicBlock {
    /// Print this block against `ctx`'s tables.
    pub fn display<'a>(&'a self, ctx: IrCtx<'a>) -> BasicBlockDisplay<'a> {
        BasicBlockDisplay { block: self, ctx }
    }
}

impl fmt::Display for BasicBlockDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Label
        if let Some(label) = &self.block.label {
            writeln!(f, "{}:", label)?;
        } else {
            writeln!(f, "{}:", self.block.id)?;
        }

        // Instructions
        for insn in &self.block.insns {
            writeln!(f, "    {}", insn.display(self.ctx))?;
        }

        Ok(())
    }
}

// Function (Entrypoint)

/// Information about a local variable for SSA conversion
#[derive(Debug, Clone)]
pub struct LocalVar {
    /// Symbol pseudo for this variable (address)
    pub sym: PseudoId,
    /// Type of the variable (interned TypeId)
    pub typ: TypeId,
    /// Block where this variable was declared (for scope-aware phi placement)
    /// Phi nodes for this variable should only be placed at blocks dominated by this block.
    pub decl_block: Option<BasicBlockId>,
    /// Explicit alignment from _Alignas specifier (C11 6.7.5)
    /// None means use natural alignment for the type
    pub explicit_align: Option<u32>,
}

impl LocalVar {
    /// Is this an ordinary object -- neither volatile anywhere inside
    /// (`contains_volatile`: a volatile member counts) nor `_Atomic` -- whose
    /// accesses a pass may promote, forward, merge or delete?
    ///
    /// Asked of the type every time rather than stored beside it, so that no
    /// constructor of a `LocalVar` can record an answer its type disagrees
    /// with. Five linearizer sites each derived the two flags themselves, and
    /// the others passed `false` for types that could be qualified.
    pub fn is_ordinary(&self, types: &TypeTable) -> bool {
        !types.contains_volatile(self.typ) && !types.is_atomic(self.typ)
    }
}

/// What each `Arg` pseudo of a function stands for: its declared parameters,
/// one `Arg` further along when the hidden struct-return pointer is `Arg(0)`.
/// See [`Function::arg_types`].
pub struct ArgTypes<'a> {
    /// The hidden struct-return pointer, if the function has one.
    pub sret: Option<PseudoId>,
    params: &'a [(String, TypeId)],
}

impl<'a> ArgTypes<'a> {
    pub fn new(sret: Option<PseudoId>, params: &'a [(String, TypeId)]) -> Self {
        ArgTypes { sret, params }
    }

    /// The type the caller passes for the parameter `Arg(arg)` carries, or
    /// `None` for the sret pointer, which is no declared parameter.
    pub fn of(&self, arg: u32) -> Option<TypeId> {
        let i = arg.checked_sub(u32::from(self.sret.is_some()))?;
        self.params.get(i as usize).map(|(_, typ)| *typ)
    }

    /// The `Arg` number the `i`-th declared parameter arrives as.
    pub fn arg_of_param(&self, i: usize) -> u32 {
        i as u32 + u32::from(self.sret.is_some())
    }
}

/// A parameter whose local storage is filled implicitly by the backend prologue
/// (e.g. complex / two-SSE struct params passed in XMM registers).
/// The inliner uses this to generate explicit struct copies at inline sites.
#[derive(Debug, Clone, Copy)]
pub struct ImplicitParamCopy {
    /// Index into the call instruction's source list
    pub arg_index: u32,
    /// Callee's local Sym pseudo that receives the data
    pub local_sym: PseudoId,
    /// Struct size in bytes
    pub size_bytes: usize,
    /// Type for the 8-byte load/store operations (typically `long`)
    pub qword_type: TypeId,
    /// Whether the caller's argument pseudo holds an *address* of the value
    /// rather than the value itself.
    ///
    /// The two kinds of parameter recorded here disagree about this, and size
    /// alone cannot tell them apart. A register-sized aggregate travels *as*
    /// its value, so `struct { float a, b; }` must be stored straight into the
    /// local. A `_Complex` travels by address at **every** size, so the
    /// eight-byte `float _Complex` -- the same size -- must be loaded through.
    /// Deciding by size stored the pointer into the local and the inlined body
    /// read it as a pair of floats.
    ///
    /// Recorded here because only the linearizer can answer it: the inliner
    /// has no `TypeTable` to ask.
    pub arg_is_address: bool,
}

/// A function in IR form
#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,
    /// `weak`, `used`, `section(...)`, `visibility(...)`.
    pub symbol_attrs: crate::parse::ast::SymbolAttrs,
    /// `__attribute__((aligned(N)))`: the byte alignment the function's code
    /// must start at, or `None` for the target's own.
    pub align: Option<u32>,
    /// Return type (interned TypeId)
    pub return_type: TypeId,
    /// Parameter names and types (interned TypeIds)
    pub params: Vec<(String, TypeId)>,
    /// All basic blocks
    pub blocks: Vec<BasicBlock>,
    /// Entry block ID
    pub entry: BasicBlockId,
    /// All pseudos indexed by PseudoId
    pub pseudos: Vec<Pseudo>,
    /// Next pseudo ID to allocate (monotonically increasing)
    pub next_pseudo: u32,
    /// Local variables (name -> info), used for SSA conversion
    pub locals: HashMap<String, LocalVar>,
    /// Is this function static (internal linkage)?
    pub is_static: bool,
    /// Whether to emit a body for this function at all.
    ///
    /// False for an *inline definition* -- C99 6.7.4p6, or its GNU
    /// counterpart selected by `__gnu_inline__`. Such a definition is fully
    /// available for inlining but provides no external definition, so emitting
    /// one puts a duplicate symbol in every object that includes the header.
    /// The function stays in the module because the inliner still needs its
    /// body; only the backends' emit loops skip it.
    pub emit: bool,
    /// Is this function noreturn (never returns)?
    pub is_noreturn: bool,
    /// The calling convention of the function's type: how its parameters
    /// arrive, its value leaves, and which registers it must preserve.
    pub conv: CallingConv,
    /// Is this function declared with the inline keyword?
    pub is_inline: bool,
    /// `__attribute__((noinline))`: the inliner must leave this function
    /// alone, whatever its size says.
    pub is_noinline: bool,
    /// The x86-64 extensions this function is compiled for: the translation
    /// unit's, or what its `target(...)` attribute or `target_clones`
    /// version asks. What the linearizer and the backend both read; set
    /// once, by `Linearizer::linearize_function`. The baseline elsewhere.
    pub isa: crate::target::X86Isa,
    /// `__attribute__((pure))` / `((const))`, as written.
    ///
    /// The programmer's promise, kept separate from anything `ir/effects.rs`
    /// derives: the inference seeds this *fixed* and never lowers it,
    /// because an attribute that in-TU analysis could overrule would buy
    /// nothing where it is most often written -- on a prototype for a
    /// function this translation unit cannot see.
    pub declared_effect: crate::parse::ast::MemEffect,
    /// Whether this function takes the address of one of its own labels.
    ///
    /// The address is a symbol naming a block of *this* function, so memory
    /// analysis gives up on it, and the inliner renames the symbol for every
    /// block it moves into a caller -- which then takes a label address of
    /// its own. Recorded here rather than recovered from symbol names,
    /// because a string literal's symbol is also spelled `.L...` and matching
    /// on the prefix silently stopped every function containing a string
    /// literal from being inlined.
    pub takes_label_addr: bool,
    /// Whether a label address of this function initializes an object of
    /// static storage duration -- `static void *tbl[] = {&&a, &&b};`.
    ///
    /// The table is one object however many copies of the body exist, and it
    /// names *this* function's blocks, so no copy of the body could use it.
    /// gcc never copies such a function ("saves address of local label in a
    /// static variable"), and neither does the inliner.
    pub saves_label_in_static: bool,
    /// `__attribute__((always_inline))`: inline at every call site regardless
    /// of size, and at `-O0` too. `is_noinline` wins if both are present.
    pub is_always_inline: bool,
    /// `__attribute__((constructor))`: emit a pointer to this function in
    /// `.init_array` so it runs before `main`. `Some(None)` is the attribute
    /// without a priority; `Some(Some(p))` carries one.
    pub constructor: Option<Option<u16>>,
    /// `__attribute__((destructor))`: the `.fini_array` counterpart of
    /// [`Function::constructor`], encoded the same way.
    pub destructor: Option<Option<u16>>,
    /// Parameters whose data is supplied implicitly by the backend prologue
    /// (e.g. complex / two-SSE struct params).  When inlining the function,
    /// the inliner must generate an explicit struct copy from the caller's
    /// address argument into the local.
    pub implicit_param_copies: Vec<ImplicitParamCopy>,
    /// Does this function return a complex value?
    ///
    /// A `_Complex` return's `Ret` carries the *address* of its halves, and
    /// the caller expects the value; bridging the two needs the base type and
    /// stride, which the optimizer has no `TypeTable` to ask for, so such a
    /// function is not inlined. An aggregate returned by address is not in
    /// this set: its `Ret` carries the ABI classification that lets the
    /// inliner copy the bytes (`Instruction::returns_aggregate_address`).
    pub ret_is_address: bool,
    /// The hidden struct-return pointer, when the function returns through
    /// one: the `Arg(0)` pseudo the linearizer creates for it, which shifts
    /// every declared parameter one `Arg` along. Read it through
    /// [`Function::sret_arg`].
    pub sret: Option<PseudoId>,
    /// Block ID -> index in `blocks` vec (O(1) lookup)
    block_idx: HashMap<BasicBlockId, usize>,
    /// Pseudo ID -> index in `pseudos` vec (O(1) lookup)
    pseudo_idx: HashMap<PseudoId, usize>,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: String::new(),
            symbol_attrs: Default::default(),
            align: None,
            takes_label_addr: false,
            saves_label_in_static: false,
            return_type: TypeId::INVALID,
            params: Vec::with_capacity(DEFAULT_PARAM_CAPACITY),
            blocks: Vec::new(),
            entry: BasicBlockId(0),
            pseudos: Vec::new(),
            next_pseudo: 0,
            locals: HashMap::new(),
            is_static: false,
            emit: true,
            is_noreturn: false,
            conv: CallingConv::C,
            is_noinline: false,
            isa: Default::default(),
            declared_effect: crate::parse::ast::MemEffect::Unknown,
            is_always_inline: false,
            constructor: None,
            destructor: None,
            is_inline: false,
            implicit_param_copies: Vec::new(),
            ret_is_address: false,
            sret: None,
            block_idx: HashMap::new(),
            pseudo_idx: HashMap::new(),
        }
    }
}

impl Function {
    pub fn new(name: impl Into<String>, return_type: TypeId) -> Self {
        Self {
            name: name.into(),
            return_type,
            ..Default::default()
        }
    }

    /// Add a parameter
    pub fn add_param(&mut self, name: impl Into<String>, typ: TypeId) {
        self.params.push((name.into(), typ));
    }

    /// Add a basic block
    pub fn add_block(&mut self, block: BasicBlock) {
        let idx = self.blocks.len();
        self.block_idx.insert(block.id, idx);
        self.blocks.push(block);
    }

    /// Where `id` sits in `blocks`.
    ///
    /// For a pass that needs the *index* rather than the block -- to index
    /// `blocks` again later, or to avoid re-borrowing. Answered from the same
    /// map `get_block` uses, so a linear scan is never the right way to find
    /// one: a pass that scans per edge is quadratic on a large function.
    pub fn block_index(&self, id: BasicBlockId) -> Option<usize> {
        self.block_idx.get(&id).copied()
    }

    /// Get a block by ID
    pub fn get_block(&self, id: BasicBlockId) -> Option<&BasicBlock> {
        self.block_idx
            .get(&id)
            .and_then(|&idx| self.blocks.get(idx))
    }

    /// Get a mutable block by ID
    pub fn get_block_mut(&mut self, id: BasicBlockId) -> Option<&mut BasicBlock> {
        self.block_idx
            .get(&id)
            .and_then(|&idx| self.blocks.get_mut(idx))
    }

    /// Drop every `Nop`: they hold nothing (`Instruction::kill` resets the
    /// whole instruction), and every pass pays to skip them.
    pub fn remove_nops(&mut self) {
        for bb in &mut self.blocks {
            bb.insns.retain(|i| i.op != Opcode::Nop);
        }
    }

    /// Add a pseudo for tracking
    pub fn add_pseudo(&mut self, pseudo: Pseudo) {
        let idx = self.pseudos.len();
        self.pseudo_idx.insert(pseudo.id, idx);
        self.pseudos.push(pseudo);
    }

    /// Is a pseudo with this id registered?
    pub fn has_pseudo(&self, id: PseudoId) -> bool {
        self.pseudo_idx.contains_key(&id)
    }

    /// Register `pseudo`, overwriting any pseudo with the same id where it
    /// stands, so no other pseudo moves and `pseudo_idx` stays right.
    ///
    /// Removing the old one and appending the new one shifted every later
    /// pseudo down a position under an index nobody rebuilt.
    pub fn replace_pseudo(&mut self, pseudo: Pseudo) {
        match self.pseudo_idx.get(&pseudo.id) {
            Some(&idx) => self.pseudos[idx] = pseudo,
            None => self.add_pseudo(pseudo),
        }
    }

    /// Rebuild block index after bulk mutation of `self.blocks`
    pub fn rebuild_block_idx(&mut self) {
        self.block_idx.clear();
        for (idx, block) in self.blocks.iter().enumerate() {
            self.block_idx.insert(block.id, idx);
        }
    }

    /// Rebuild the pseudo index after bulk mutation of `self.pseudos`.
    ///
    /// `pseudo_idx` maps an id to a *position*, so removing any element
    /// invalidates every later entry, not just the removed one.
    pub fn rebuild_pseudo_idx(&mut self) {
        self.pseudo_idx.clear();
        for (idx, pseudo) in self.pseudos.iter().enumerate() {
            self.pseudo_idx.insert(pseudo.id, idx);
        }
    }

    /// Add a local variable
    pub fn add_local(
        &mut self,
        name: impl Into<String>,
        sym: PseudoId,
        typ: TypeId,
        decl_block: Option<BasicBlockId>,
        explicit_align: Option<u32>,
    ) {
        self.locals.insert(
            name.into(),
            LocalVar {
                sym,
                typ,
                decl_block,
                explicit_align,
            },
        );
    }

    /// Get a local variable
    pub fn get_local(&self, name: &str) -> Option<&LocalVar> {
        self.locals.get(name)
    }

    /// Does this function contain a `__builtin_setjmp`, the receiver of a
    /// non-local goto? gcc never copies such a function, and its prologue
    /// saves every callee-saved register: a `__builtin_longjmp` skips the
    /// epilogues of the frames it unwinds, so whatever they changed is still
    /// changed when control resumes here.
    pub fn receives_nonlocal_goto(&self) -> bool {
        self.blocks
            .iter()
            .flat_map(|b| &b.insns)
            .any(Instruction::is_builtin_setjmp)
    }

    /// The local variable that `sym` *is*, if it is one.
    ///
    /// Asking `locals` by name cannot answer this: a parameter is registered
    /// under its bare name, and a global reached through a block-scope
    /// `extern` gets its own pseudo carrying the same name, so a name matches
    /// two different objects. Only block-scope locals are mangled `name.<id>`
    /// and so cannot collide. Answering by pseudo identity is what keeps a
    /// global from being handed the parameter's stack slot, and a thread-local
    /// from being mistaken for one and left unexpanded.
    ///
    /// Every "is this `Sym` a local" question asks here, the thread-local
    /// expansion and the backend's check of it included.
    pub fn local_of(&self, sym: PseudoId) -> Option<&LocalVar> {
        self.get_pseudo(sym)
            .and_then(|p| match &p.kind {
                PseudoKind::Sym(name) => self.locals.get(name),
                _ => None,
            })
            .filter(|local| local.sym == sym)
    }

    /// The pseudo standing for each incoming argument, by `Arg` index -- the
    /// first, if several claim one.
    ///
    /// Built once for a walk over the parameters: finding each parameter's
    /// pseudo by scanning `pseudos` made that walk parameters x pseudos.
    pub fn arg_pseudos(&self) -> HashMap<u32, &Pseudo> {
        let mut by_arg = HashMap::new();
        for pseudo in &self.pseudos {
            if let PseudoKind::Arg(idx) = pseudo.kind {
                by_arg.entry(idx).or_insert(pseudo);
            }
        }
        by_arg
    }

    /// The hidden struct-return pointer, if this function has one.
    ///
    /// The linearizer emits it as `Arg(0)` and records it in
    /// [`Function::sret`]; it shifts every declared parameter one `Arg` along.
    pub fn sret_arg(&self) -> Option<PseudoId> {
        self.sret
    }

    /// The pseudos an inline `asm` writes as outputs.
    ///
    /// An asm output is a second definition of its pseudo, the one invariant
    /// I1 deliberately exempts, so for these "the instruction that defines
    /// %n" says nothing about the value: a tied operand (`"0"(x)`) is even
    /// written as a `Copy` into the output pseudo *before* the asm. Every
    /// pass that follows a pseudo to its definition, or folds a use of it,
    /// leaves these alone; asking here keeps them answering alike, because a
    /// pass that folds what the others would not is the one that miscompiles.
    pub fn asm_defined_pseudos(&self) -> HashSet<PseudoId> {
        self.blocks
            .iter()
            .flat_map(|bb| &bb.insns)
            .filter_map(|insn| insn.extra().asm_data.as_deref())
            .flat_map(|asm| asm.outputs.iter().map(|o| o.pseudo))
            .collect()
    }

    /// The type the caller passes for the parameter an `Arg(arg)` pseudo
    /// carries, or `None` for the hidden sret pointer, which is no declared
    /// parameter.
    ///
    /// `params[arg]` is the answer only without an sret pointer; with one,
    /// every parameter is one `Arg` further along. Indexing the list directly
    /// took the *next* parameter's type for each of them.
    pub fn param_type_of_arg(&self, arg: u32) -> Option<TypeId> {
        self.arg_types().of(arg)
    }

    /// [`Function::param_type_of_arg`] for many arguments, and the `Arg`
    /// each declared parameter arrives as.
    pub fn arg_types(&self) -> ArgTypes<'_> {
        ArgTypes::new(self.sret_arg(), &self.params)
    }

    /// The type of the value an `Arg` pseudo holds, when it holds one.
    ///
    /// A scalar parameter arrives as its value, at the width of the type it
    /// is passed as, and that is what the entry block stores into its slot.
    /// An aggregate, a complex number or a `va_list` may arrive as an address
    /// or as the storage itself, so its `Arg` is no value of its declared
    /// type and this answers `None` for it.
    pub fn arg_value_type(&self, id: PseudoId, types: &TypeTable) -> Option<TypeId> {
        let Some(PseudoKind::Arg(n)) = self.get_pseudo(id).map(|p| &p.kind) else {
            return None;
        };
        self.param_type_of_arg(*n)
            .filter(|&t| types.is_scalar(t) && !types.is_complex(t))
    }

    /// Allocate a new pseudo ID
    /// Returns a unique ID and increments the counter
    pub fn alloc_pseudo(&mut self) -> PseudoId {
        let id = PseudoId(self.next_pseudo);
        self.next_pseudo += 1;
        id
    }

    /// Create a new register pseudo and return its ID.
    /// The pseudo is added to self.pseudos.
    pub fn create_reg_pseudo(&mut self) -> PseudoId {
        let id = self.alloc_pseudo();
        self.add_pseudo(Pseudo::reg(id, id.0));
        id
    }

    /// Is `id` an ordinary SSA temporary -- a value and nothing more?
    ///
    /// True for an id that is not in `pseudos` at all, which is most of them:
    /// `Linearizer::alloc_pseudo` records nothing, so a plain register is
    /// exactly what an absent id means. False for an `Arg`, a `Phi`, a `Sym`
    /// or an existing constant, each of which carries a meaning beyond its
    /// value that a rewrite must not take away.
    pub fn is_plain_temp(&self, id: PseudoId) -> bool {
        match self.get_pseudo(id) {
            Some(p) => matches!(p.kind, PseudoKind::Reg(_)),
            None => true,
        }
    }

    /// Make `id` a constant pseudo holding `value`.
    ///
    /// A constant is a pseudo *kind*, so folding a value into one works the
    /// other way round from rewriting an instruction: the target is
    /// converted in place, keeping its identity so that every use already
    /// names it, and its defining instruction becomes the `SetVal` that
    /// gives it a width.
    ///
    /// `false`, and nothing done, for an id [`Self::is_plain_temp`] rejects.
    ///
    /// Half of a rewrite: `propagate::fold_target_to_setval` is the whole of
    /// it, and the only caller, because the `SetVal` is not optional. An
    /// `FVal` without one is resolved at a default width of 64 bits, so a
    /// folded `float` would be read out of eight bytes; for an integer it
    /// decides the stack slot in x86-64's sixteen-byte case.
    fn make_const(&mut self, id: PseudoId, value: ConstValue) -> bool {
        if !self.is_plain_temp(id) {
            return false;
        }
        let kind = match value {
            ConstValue::Int(v) => PseudoKind::Val(v),
            ConstValue::Float(v) => PseudoKind::FVal(v),
        };
        match self.pseudo_idx.get(&id).copied() {
            Some(idx) => match self.pseudos.get_mut(idx) {
                Some(p) => p.kind = kind,
                None => return false,
            },
            None => self.add_pseudo(Pseudo {
                id,
                kind,
                name: None,
            }),
        }
        true
    }

    /// Create a new constant integer pseudo and return its ID.
    /// The pseudo is added to self.pseudos.
    pub fn create_const_pseudo(&mut self, value: i128) -> PseudoId {
        let id = self.alloc_pseudo();
        let pseudo = Pseudo::val(id, value);
        self.add_pseudo(pseudo);
        id
    }

    /// Get a pseudo by its ID
    pub fn get_pseudo(&self, id: PseudoId) -> Option<&Pseudo> {
        self.pseudo_idx
            .get(&id)
            .and_then(|&idx| self.pseudos.get(idx))
    }

    /// Get the constant integer value of a pseudo, if it is a Val.
    pub fn const_val(&self, id: PseudoId) -> Option<i128> {
        self.get_pseudo(id).and_then(|p| match &p.kind {
            PseudoKind::Val(v) => Some(*v),
            _ => None,
        })
    }

    /// Get the symbol name of a pseudo, if it is a Sym.
    pub fn sym_name_of(&self, id: PseudoId) -> Option<&str> {
        self.get_pseudo(id).and_then(|p| match &p.kind {
            PseudoKind::Sym(name) => Some(name.as_str()),
            _ => None,
        })
    }

    /// The global a `Sym` pseudo names, if it names one.
    ///
    /// A local's `Sym` carries the local's name, which may be spelled like a
    /// global's -- a parameter `count` beside a function `count` -- so a
    /// lookup of global names by `sym_name_of` alone mistakes the one for the
    /// other. Decided by identity, through [`Self::local_of`].
    pub fn global_sym_name(&self, id: PseudoId) -> Option<&str> {
        self.sym_name_of(id).filter(|_| self.local_of(id).is_none())
    }
}

/// A scalar constant a pseudo can be turned into.
///
/// The two cases are not interchangeable and never inferred from a width:
/// which one a value is decides the register file it lives in.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstValue {
    Int(i128),
    Float(FloatVal),
}

/// A `Function` paired with the type table.
pub struct FunctionDisplay<'a> {
    func: &'a Function,
    types: &'a TypeTable,
}

impl Function {
    /// Print this function, resolving its pseudos and types.
    pub fn display<'a>(&'a self, types: &'a TypeTable) -> FunctionDisplay<'a> {
        FunctionDisplay { func: self, types }
    }
}

impl fmt::Display for FunctionDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func = self.func;
        // The function is its own pseudo table: every id in its body resolves
        // against it, and against no other function's.
        let ctx = IrCtx {
            types: self.types,
            func: Some(func),
        };

        // Function header
        write!(f, "define {} {}(", ctx.typ(func.return_type), func.name)?;
        for (i, (name, typ)) in func.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} %{}", ctx.typ(*typ), name)?;
        }
        writeln!(f, ") {{")?;

        // Basic blocks
        for block in &func.blocks {
            write!(f, "{}", block.display(ctx))?;
        }

        writeln!(f, "}}")
    }
}

// Global Variable Initializer

/// Initializer for global variables
#[derive(Debug, Clone, PartialEq, Default)]
pub enum Initializer {
    /// No initializer (zero-initialized)
    #[default]
    None,
    /// Integer initializer
    Int(i128),
    /// Float/double initializer
    Float(FloatVal),
    /// An IEEE binary128 initializer.
    ///
    /// Distinct from `Float` because the 16-byte encoding is not decided by
    /// the width: on x86-64 a 16-byte float initializer is x87's 80-bit image
    /// unless the object is a `__float128`. The type knows; the byte count
    /// does not, so the type records it here.
    Float128(FloatVal),
    /// String literal initializer (for char arrays)
    String(String),
    /// A `u"..."` initializer: char16_t code units.
    Utf16String(Vec<u16>),
    /// A `U"..."` or `L"..."` initializer: 4-byte code units, which is what
    /// both `char32_t` and `wchar_t` are on every target.
    Utf32String(Vec<u32>),
    /// Array initializer: element size in bytes, list of (offset, initializer) pairs
    /// Elements not listed are zero-initialized
    Array {
        elem_size: usize,
        total_size: usize,
        elements: Vec<(usize, Initializer)>,
    },
    /// Struct initializer: list of (offset, size, initializer) tuples
    /// Fields not listed are zero-initialized
    Struct {
        total_size: usize,
        /// Each tuple is (offset, field_size, initializer)
        fields: Vec<(usize, usize, Initializer)>,
    },
    /// Address of a symbol (for pointer initializers like `int *p = &x;`)
    SymAddr(String),
    /// Address of a symbol plus offset (for pointer initializers like `int *p = &s.field;`)
    SymAddrOffset(String, i64),
    /// GNU `&&end - &&start + addend`: the distance in bytes between two
    /// labels of one function, which the assembler writes as a symbol
    /// difference at the object's width. Both are block-label symbols.
    LabelDiff {
        end: String,
        start: String,
        addend: i64,
    },
}

impl Initializer {
    /// A string literal initializing an array of `total_size` bytes, as the
    /// element list it stands for: one `Int` per code unit that fits, each
    /// `elem_size` bytes wide. `None` for anything that is not a string.
    ///
    /// A literal is one initializer for the whole array, so a later
    /// designator naming one of its elements -- `{ .s = "abc", .s[1] = 'z' }`
    /// -- has nothing to replace until the literal is seen as the elements
    /// it is. Units past the array are cut, as the literal itself is.
    pub fn string_as_array(&self, elem_size: usize, total_size: usize) -> Option<Initializer> {
        let units: Vec<i128> = match self {
            Initializer::String(s) => crate::token::lexer::payload_bytes(s)
                .map(i128::from)
                .collect(),
            Initializer::Utf16String(u) => u.iter().map(|&c| i128::from(c)).collect(),
            Initializer::Utf32String(u) => u.iter().map(|&c| i128::from(c)).collect(),
            _ => return None,
        };
        let fits = total_size.checked_div(elem_size).unwrap_or(0);
        Some(Initializer::Array {
            elem_size,
            total_size,
            elements: units
                .into_iter()
                .take(fits)
                .enumerate()
                .map(|(i, u)| (i * elem_size, Initializer::Int(u)))
                .collect(),
        })
    }

    /// Call `f` with every symbol this initializer names: the target of an
    /// address and both labels of a label difference.
    pub fn for_each_symbol(&self, f: &mut impl FnMut(&str)) {
        match self {
            Initializer::SymAddr(name) | Initializer::SymAddrOffset(name, _) => f(name),
            Initializer::LabelDiff { end, start, .. } => {
                f(end);
                f(start);
            }
            Initializer::Array { elements, .. } => {
                for (_, init) in elements {
                    init.for_each_symbol(f);
                }
            }
            Initializer::Struct { fields, .. } => {
                for (_, _, init) in fields {
                    init.for_each_symbol(f);
                }
            }
            Initializer::None
            | Initializer::Int(_)
            | Initializer::Float(_)
            | Initializer::Float128(_)
            | Initializer::String(_)
            | Initializer::Utf16String(_)
            | Initializer::Utf32String(_) => {}
        }
    }

    /// Recursively determine whether this initializer evaluates to all zero bytes.
    ///
    /// Used to route static / extern globals whose initial contents are entirely
    /// zero into `.bss` (which costs nothing in the object file and is lazily
    /// allocated by the kernel) instead of `.data` (which pays in both file size
    /// and resident memory).
    pub fn is_all_zero(&self) -> bool {
        match self {
            Initializer::None => true,
            Initializer::Int(v) => *v == 0,
            Initializer::Float(v) | Initializer::Float128(v) => v.is_positive_zero(),
            // A zero-length string is all-zero; a non-empty char array initialized
            // by a string literal is zero iff every byte is `\0`.
            Initializer::String(s) => s.chars().all(|c| c == '\0'),
            Initializer::Utf16String(u) => u.iter().all(|&c| c == 0),
            Initializer::Utf32String(u) => u.iter().all(|&c| c == 0),
            Initializer::Array { elements, .. } => {
                elements.iter().all(|(_, init)| init.is_all_zero())
            }
            Initializer::Struct { fields, .. } => {
                fields.iter().all(|(_, _, init)| init.is_all_zero())
            }
            // Address-of expressions are never zero — they take an address.
            // A label difference is not known until the function is
            // assembled, so it needs data of its own either way.
            Initializer::SymAddr(_)
            | Initializer::SymAddrOffset(_, _)
            | Initializer::LabelDiff { .. } => false,
        }
    }

    /// Recursively determine whether this initializer references any symbol
    /// address (and therefore needs runtime relocation).
    ///
    /// Used to distinguish `.rodata` (pure read-only data) from `.data.rel.ro`
    /// (read-only data containing pointers that the dynamic linker fixes up).
    pub fn has_reloc(&self) -> bool {
        match self {
            Initializer::SymAddr(_) | Initializer::SymAddrOffset(_, _) => true,
            Initializer::Array { elements, .. } => {
                elements.iter().any(|(_, init)| init.has_reloc())
            }
            Initializer::Struct { fields, .. } => {
                fields.iter().any(|(_, _, init)| init.has_reloc())
            }
            // Two labels of one section: the assembler resolves their
            // distance, and nothing is left for the loader to fix up.
            Initializer::LabelDiff { .. }
            | Initializer::Float128(_)
            | Initializer::None
            | Initializer::Int(_)
            | Initializer::Float(_)
            | Initializer::String(_)
            | Initializer::Utf16String(_)
            | Initializer::Utf32String(_) => false,
        }
    }
}

impl fmt::Display for Initializer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Initializer::None => write!(f, "0"),
            Initializer::Int(v) => write!(f, "{}", v),
            Initializer::Float(v) | Initializer::Float128(v) => write!(f, "{}", v),
            Initializer::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Initializer::Utf16String(u) => write!(f, "u\"<{} units>\"", u.len()),
            Initializer::Utf32String(u) => write!(f, "U\"<{} units>\"", u.len()),
            Initializer::Array {
                total_size,
                elements,
                ..
            } => {
                write!(f, "[{}]{{ ", total_size)?;
                for (i, (off, init)) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "+{}: {}", off, init)?;
                }
                write!(f, " }}")
            }
            Initializer::Struct { total_size, fields } => {
                write!(f, "struct({}){{ ", total_size)?;
                for (i, (off, size, init)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "+{}[{}]: {}", off, size, init)?;
                }
                write!(f, " }}")
            }
            Initializer::SymAddr(name) => write!(f, "&{}", name),
            Initializer::LabelDiff { end, start, addend } => {
                write!(f, "&&{}-&&{}", end, start)?;
                if *addend != 0 {
                    write!(f, "{:+}", addend)?;
                }
                Ok(())
            }
            Initializer::SymAddrOffset(name, offset) => {
                if *offset >= 0 {
                    write!(f, "&{}+{}", name, offset)
                } else {
                    write!(f, "&{}{}", name, offset)
                }
            }
        }
    }
}

// Global Variable Definition

/// How a global is stored, as three facts that always travel together.
///
/// Passed to [`Module::define_global`] as one value because three adjacent
/// booleans at a call site say nothing about which is which.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct GlobalStorage {
    /// Internal linkage.
    pub(crate) is_static: bool,
    /// A `const`-qualified object: see `GlobalDef::is_const`.
    pub(crate) is_const: bool,
    /// C11 `_Thread_local` / GCC `__thread`.
    pub(crate) is_thread_local: bool,
}

/// A global variable definition with full metadata
#[derive(Debug, Clone)]
pub struct GlobalDef {
    /// Variable name
    pub name: String,
    /// Variable type
    pub typ: TypeId,
    /// Initializer (None for uninitialized)
    pub init: Initializer,
    /// C11 _Thread_local / GCC __thread
    pub is_thread_local: bool,
    /// Static storage class (internal linkage)
    pub is_static: bool,
    /// Top-level `const`-qualified declaration. Used to route the global to
    /// `.rodata` (no relocations) or `.data.rel.ro` (relocations) instead of
    /// the writable `.data` section.
    pub is_const: bool,
    /// Explicit alignment from _Alignas specifier (None = use natural alignment)
    pub explicit_align: Option<u32>,
    /// `weak`, `used`, `section(...)`, `visibility(...)`.
    pub symbol_attrs: crate::parse::ast::SymbolAttrs,
}

impl GlobalDef {
    /// Create a new global variable definition
    pub fn new(name: impl Into<String>, typ: TypeId, init: Initializer) -> Self {
        Self {
            name: name.into(),
            typ,
            init,
            is_thread_local: false,
            is_static: false,
            is_const: false,
            explicit_align: None,
            symbol_attrs: Default::default(),
        }
    }

    /// Set explicit alignment from _Alignas specifier
    pub fn with_align(mut self, align: Option<u32>) -> Self {
        self.explicit_align = align;
        self
    }

    /// Set static storage class (internal linkage)
    pub fn with_static(mut self, is_static: bool) -> Self {
        self.is_static = is_static;
        self
    }

    /// Set top-level const qualification
    pub fn with_const(mut self, is_const: bool) -> Self {
        self.is_const = is_const;
        self
    }
}

/// A second name for a definition in this translation unit, from
/// `__attribute__((alias("target")))`.
///
/// Neither a definition nor a reference: the alias owns no storage and no
/// code, and the object file gets `name` as a symbol whose value is
/// `target`'s address -- `.set name, target`. It has its own binding, which
/// is why it is not folded into the target: `static` makes it local, `weak`
/// lets a strong definition elsewhere replace it while `target` keeps its own
/// name, and visibility is set on it alone.
///
/// Two names for one object is exactly what the memory passes must not
/// assume away. An alias is never in `Module::globals`, so `memloc` knows
/// nothing about it and answers "may alias anything" for every access made
/// through it -- which is what makes a store through `b` visible to a load of
/// `a`. The target must stay emitted even when nothing else names it:
/// `inline::remove_dead_functions` counts an alias as a reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolAlias {
    /// The alias, as the assembler spells it.
    pub name: String,
    /// The symbol it names: a definition in this unit, or another alias.
    /// For an `ifunc`, the resolver.
    pub target: String,
    /// A second name for `target`, or a GNU indirect function `target`
    /// resolves.
    pub form: crate::parse::ast::AliasForm,
    /// Declared `static`: a local symbol, no `.globl`.
    pub is_static: bool,
    /// `weak`: `.weak` rather than `.globl`.
    pub weak: bool,
    /// `visibility("...")`, verbatim, as on any other symbol.
    pub visibility: Option<String>,
}

// Module (Translation Unit)

impl Module {
    /// `-fvisibility=how`: give every definition with external linkage that
    /// asked for no visibility of its own -- function, object or alias --
    /// this one. A declaration of something defined elsewhere is not
    /// affected, as in gcc, and `visibility(...)` on the symbol still wins.
    ///
    /// Ignoring the flag exported every symbol of a shared object built with
    /// `-fvisibility=hidden`, and its calls between its own functions then
    /// bound, through the PLT, to whatever the executable exported under the
    /// same names: a CPython extension carrying its own copy of the parser
    /// ran the interpreter's instead.
    pub fn apply_default_visibility(&mut self, how: &str) {
        if how == "default" {
            return;
        }
        let set = |slot: &mut Option<String>, is_static: bool| {
            if !is_static && slot.is_none() {
                *slot = Some(how.to_string());
            }
        };
        for func in &mut self.functions {
            set(&mut func.symbol_attrs.visibility, func.is_static);
        }
        for global in &mut self.globals {
            set(&mut global.symbol_attrs.visibility, global.is_static);
        }
        for alias in &mut self.aliases {
            set(&mut alias.visibility, alias.is_static);
        }
    }
}

/// A module containing multiple functions
#[derive(Debug, Clone, Default)]
pub struct Module {
    /// Functions
    pub functions: Vec<Function>,
    /// Global variables
    pub globals: Vec<GlobalDef>,
    /// String literals (label, content)
    pub strings: Vec<(String, String)>,
    /// `u"..."` literals referenced by address, as char16_t code units.
    pub utf16_strings: Vec<(String, Vec<u16>)>,
    /// `U"..."` and `L"..."` literals referenced by address, as 4-byte code
    /// units.
    pub utf32_strings: Vec<(String, Vec<u32>)>,
    /// Generate debug info
    pub debug: bool,
    /// Source file paths (stream id -> path) for .file directives
    pub source_files: Vec<String>,
    /// External symbols (declared extern but not defined in this module)
    /// These need GOT access on macOS
    pub extern_symbols: HashSet<String>,
    /// Attributes written on a symbol this translation unit declares but does
    /// not define. `weak` is the point of it: `extern int f(void)
    /// __attribute__((weak));` has to reach the object file as `.weak f`, or
    /// the reference is strong and an absent definition is a link error
    /// instead of a null pointer -- which is the entire idiom.
    ///
    /// Ordered, because it is iterated to emit directives.
    pub declared_symbol_attrs: std::collections::BTreeMap<String, crate::parse::ast::SymbolAttrs>,
    /// `__attribute__((pure))` / `((const))` on a function this translation
    /// unit declares but does not define.
    ///
    /// For most of what a program calls, the prototype is all there is:
    /// glibc's `__pure__ strlen` is the only thing that says `strlen` writes
    /// nothing, and without it every call to it is a full memory barrier.
    pub declared_fn_effects: std::collections::BTreeMap<String, crate::parse::ast::MemEffect>,
    /// External thread-local symbols (declared extern _Thread_local but not defined)
    /// These need TLS access pattern instead of GOT
    pub extern_tls_symbols: HashSet<String>,
    /// The alignment, in bytes, of each data object declared `extern` here and
    /// not defined: at least its declared type's, which is all an object
    /// defined elsewhere is known to have. A backend that folds a symbol's low
    /// bits into a scaled load or store (aarch64 `:lo12:`) needs it; a
    /// definition's alignment is on its `GlobalDef`.
    pub extern_object_align: HashMap<String, u32>,
    /// `__attribute__((alias))` symbols, in declaration order, each checked
    /// to name something this unit defines.
    pub aliases: Vec<SymbolAlias>,
    /// Compilation directory (for DW_AT_comp_dir in DWARF)
    pub comp_dir: Option<String>,
    /// Primary source filename (for DW_AT_name in DWARF)
    pub source_name: Option<String>,
    /// Each function in [`FOLD_CALLEES`] a fold may call, and the assembler
    /// name this unit's declarations give it
    /// (`Linearizer::library_function_name`): a call an optimizer pass makes
    /// to `strchr` in place of the program's `strstr` is still a call to
    /// `strchr`, asm label and all.
    ///
    /// A name is *absent* when the program bound it to something that is not
    /// a function -- `int puts;` -- and no fold may then reach for it, since
    /// the call would go to the program's own object. So this answers both
    /// "may a fold call this?" and "by what name?".
    pub library_symbols: HashMap<&'static str, String>,
    /// Where each global is in `globals`, by name: see `Module::global_mut`.
    global_idx: AppendIndex,
    /// Where each function is in `functions`, by name: see
    /// `Module::add_function`.
    function_idx: AppendIndex,
    /// Where each literal is in `strings`, by contents: see
    /// `Module::add_string`.
    string_idx: AppendIndex,
}

/// The position of each item in one of `Module`'s lists, by a key the item
/// carries: a global's or a function's name, a literal's contents.
///
/// Those lists are only ever appended to, by this module and by the passes
/// that push to them directly, so the index catches up with whatever was
/// appended since it last looked, and each item is indexed once. A hit is
/// checked against the key it claims, and a mismatch -- which an append
/// cannot cause -- rebuilds the whole index rather than answering wrongly.
#[derive(Debug, Clone, Default)]
struct AppendIndex {
    pos: HashMap<String, usize>,
    indexed: usize,
}

impl AppendIndex {
    /// The position of the first item in `items` whose `key` is `want`.
    fn find<T>(&mut self, items: &[T], key: impl Fn(&T) -> &str, want: &str) -> Option<usize> {
        if self.indexed > items.len() {
            *self = AppendIndex::default();
        }
        for (i, item) in items.iter().enumerate().skip(self.indexed) {
            // The first item with a key is the one a lookup finds.
            self.pos.entry(key(item).to_string()).or_insert(i);
        }
        self.indexed = items.len();
        let i = *self.pos.get(want)?;
        if key(&items[i]) == want {
            return Some(i);
        }
        *self = AppendIndex::default();
        self.find(items, key, want)
    }
}

/// The C library functions an optimizer pass may call where the program
/// called something else -- `strstr(s, "c")` becomes `strchr(s, 'c')` --
/// by their C names.
pub const FOLD_CALLEES: &[&str] = &[
    "strlen", "strchr", "strcpy", "memcpy", "memset", "puts", "putchar", "fputs", "fputc", "fwrite",
];

/// [`Module::strings`] and its index, borrowed apart from the rest of the
/// module so that a pass rewriting its functions can add literals.
pub struct StringPool<'a> {
    strings: &'a mut Vec<(String, String)>,
    idx: &'a mut AppendIndex,
}

impl StringPool<'_> {
    /// The label of the literal holding `content`, added if there is none.
    pub fn add(&mut self, content: String) -> String {
        // One label per distinct contents. Minting a fresh one per occurrence
        // made two identical literals two objects, which C17 6.4.5p7 permits
        // but which no compiler does -- and it made `&"Foobar"[1] -
        // &"Foobar"[0]` a difference between *different* symbols, so the
        // static initializer could not be folded at all.
        let found = self
            .idx
            .find(self.strings, |(_, c)| c.as_str(), content.as_str());
        if let Some(i) = found {
            return self.strings[i].0.clone();
        }
        let label = string_label(self.strings.len());
        self.strings.push((label.clone(), content));
        label
    }
}

/// The label of the `index`th string literal in [`Module::strings`].
pub(crate) fn string_label(index: usize) -> String {
    format!(".LC{index}")
}

impl Module {
    /// Add a function definition, keeping one function per name.
    ///
    /// A GNU inline-only body (`emit == false`) may be followed by the
    /// translation unit's real definition of the same name -- the parser
    /// allows that order and no other -- and the real one replaces it: it is
    /// the function, for calls, for the inliner and for `&f`, as in gcc. Two
    /// entries under one name would leave every lookup by name to pick one.
    pub fn add_function(&mut self, func: Function) {
        let found = self
            .function_idx
            .find(&self.functions, |f| f.name.as_str(), &func.name);
        match found {
            Some(i) if !self.functions[i].emit => self.functions[i] = func,
            _ => self.functions.push(func),
        }
    }

    /// Add a global variable
    pub fn add_global(&mut self, name: impl Into<String>, typ: TypeId, init: Initializer) {
        self.globals.push(GlobalDef::new(name, typ, init));
    }

    /// The global named `name`, without a walk of every global: a unit with
    /// 100,000 of them spent fifteen seconds finding each one.
    fn global_mut(&mut self, name: &str) -> Option<&mut GlobalDef> {
        let i = self
            .global_idx
            .find(&self.globals, |g| g.name.as_str(), name)?;
        Some(&mut self.globals[i])
    }

    /// Attach the symbol-emission attributes to a global already added.
    ///
    /// Set after the fact rather than passed to `define_global`, because only
    /// a file-scope definition carries any; a static local has none to pass.
    pub fn set_symbol_attrs(&mut self, name: &str, attrs: crate::parse::ast::SymbolAttrs) {
        if attrs.is_empty() {
            return;
        }
        if let Some(g) = self.global_mut(name) {
            g.symbol_attrs.merge(&attrs);
        }
    }

    /// Record attributes on a symbol declared but not defined here, folded
    /// together over every such declaration. A later definition in the same
    /// unit takes them over: see `take_declared_symbol_attrs`.
    pub fn set_declared_symbol_attrs(&mut self, name: &str, attrs: crate::parse::ast::SymbolAttrs) {
        if attrs.is_empty() {
            return;
        }
        self.declared_symbol_attrs
            .entry(name.to_string())
            .or_default()
            .merge(&attrs);
    }

    /// Define a global, with its explicit alignment (C11 `_Alignas`) if it
    /// has one.
    ///
    /// A C tentative definition is completed rather than duplicated: if a
    /// global of the same name exists with `Initializer::None`, this
    /// definition replaces it, and a tentative definition after an
    /// initialized one is merged into it.
    ///
    /// `typ` is the object's type alone: its storage class is in `storage`,
    /// and the parser gives every declarator of static storage duration its
    /// type without one, so `static int x; extern int x = 7;` is one `int`.
    /// The declarations of one object have compatible, identically qualified
    /// types (C17 6.2.7p2); a later one may complete an earlier one (`int
    /// (*p)[]; int (*p)[3] = 0;`), so the definition's type is the global's.
    pub(crate) fn define_global(
        &mut self,
        types: &TypeTable,
        name: impl Into<String>,
        typ: TypeId,
        init: Initializer,
        align: Option<u32>,
        storage: GlobalStorage,
    ) {
        let GlobalStorage {
            is_static,
            is_const,
            is_thread_local,
        } = storage;
        let name = name.into();
        debug_assert!(
            !types
                .modifiers(typ)
                .intersects(crate::types::Type::DECL_SPECIFIERS),
            "global '{name}' typed with its declaration's specifiers"
        );
        // Check for existing tentative definition
        if let Some(existing) = self.global_mut(&name) {
            // Replace tentative definition with actual definition
            if matches!(existing.init, Initializer::None) {
                debug_assert!(
                    types.types_compatible_qualified(existing.typ, typ),
                    "tentative definition type mismatch for '{name}'"
                );
                existing.typ = typ;
                existing.init = init;
                existing.is_static = is_static;
                // Const-ness is a property of the declaration that ultimately
                // provides storage: if either the tentative or the defining
                // declaration declared the object `const`, the resulting
                // object is treated as read-only.
                existing.is_const = existing.is_const || is_const;
                if is_thread_local {
                    existing.is_thread_local = true;
                }
                if align.is_some() {
                    existing.explicit_align = align;
                }
                return;
            }
            // A tentative definition after the definition refers to it
            // (6.9.2p2): `int y = 5; int y;` is one `y`.
            if matches!(init, Initializer::None) {
                existing.explicit_align = existing.explicit_align.max(align);
                return;
            }
        }
        let mut def = GlobalDef::new(name, typ, init)
            .with_align(align)
            .with_static(is_static)
            .with_const(is_const);
        if is_thread_local {
            def.is_thread_local = true;
        }
        self.globals.push(def);
    }

    /// Add a string literal and return its label
    pub fn add_string(&mut self, content: String) -> String {
        self.string_pool().add(content)
    }

    fn string_pool(&mut self) -> StringPool<'_> {
        StringPool {
            strings: &mut self.strings,
            idx: &mut self.string_idx,
        }
    }

    /// The functions, for a pass to rewrite, beside the literal pool the
    /// rewrite may add to and the library functions it may call
    /// (`library_symbols`).
    pub fn split_for_rewrite(
        &mut self,
    ) -> (
        &mut Vec<Function>,
        StringPool<'_>,
        &HashMap<&'static str, String>,
    ) {
        let pool = StringPool {
            strings: &mut self.strings,
            idx: &mut self.string_idx,
        };
        (&mut self.functions, pool, &self.library_symbols)
    }

    /// Intern a `u"..."` literal and return its label.
    pub fn add_utf16_string(&mut self, units: Vec<u16>) -> String {
        let label = format!(".LU16C{}", self.utf16_strings.len());
        self.utf16_strings.push((label.clone(), units));
        label
    }

    /// Intern a `U"..."` or `L"..."` literal and return its label.
    pub fn add_utf32_string(&mut self, units: Vec<u32>) -> String {
        let label = format!(".LU32C{}", self.utf32_strings.len());
        self.utf32_strings.push((label.clone(), units));
        label
    }
}

/// A `Module` paired with the type table.
pub struct ModuleDisplay<'a> {
    module: &'a Module,
    types: &'a TypeTable,
}

impl Module {
    /// Print this module, resolving its pseudos and types.
    pub fn display<'a>(&'a self, types: &'a TypeTable) -> ModuleDisplay<'a> {
        ModuleDisplay {
            module: self,
            types,
        }
    }
}

impl fmt::Display for ModuleDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // A global belongs to no function, so its pseudo table is empty; its
        // type still resolves.
        let ctx = IrCtx {
            types: self.types,
            func: None,
        };

        // Globals
        for global in &self.module.globals {
            let tls_marker = if global.is_thread_local { " [tls]" } else { "" };
            match &global.init {
                Initializer::None => {
                    writeln!(f, "@{}: {}{}", global.name, ctx.typ(global.typ), tls_marker)?
                }
                init => writeln!(
                    f,
                    "@{}: {} = {}{}",
                    global.name,
                    ctx.typ(global.typ),
                    init,
                    tls_marker
                )?,
            }
        }

        if !self.module.globals.is_empty() {
            writeln!(f)?;
        }

        // Functions
        for func in &self.module.functions {
            writeln!(f, "{}", func.display(self.types))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::abi::{ArgClass, RegClass};
    use crate::target::{Arch, Target};
    use crate::types::{Type, TypeTable};

    /// `-fvisibility=` reaches every external definition that named none,
    /// and nothing else.
    #[test]
    fn default_visibility_applies_to_external_definitions_only() {
        let types = TypeTable::new(&Target::host());
        let mut public = Function::new("public", types.int_id);
        public.symbol_attrs.visibility = Some("default".into());
        let plain = Function::new("plain", types.int_id);
        let mut local = Function::new("local", types.int_id);
        local.is_static = true;
        let mut module = Module {
            functions: vec![public, plain, local],
            globals: vec![GlobalDef::new("g", types.int_id, Initializer::None)],
            aliases: vec![SymbolAlias {
                name: "a".into(),
                target: "plain".into(),
                form: crate::parse::ast::AliasForm::Alias,
                is_static: false,
                weak: false,
                visibility: None,
            }],
            ..Default::default()
        };

        module.apply_default_visibility("hidden");
        let vis: Vec<Option<&str>> = module
            .functions
            .iter()
            .map(|f| f.symbol_attrs.visibility.as_deref())
            .collect();
        assert_eq!(vis, [Some("default"), Some("hidden"), None]);
        assert_eq!(
            module.globals[0].symbol_attrs.visibility.as_deref(),
            Some("hidden")
        );
        assert_eq!(module.aliases[0].visibility.as_deref(), Some("hidden"));

        let mut untouched = Module {
            functions: vec![Function::new("f", types.int_id)],
            ..Default::default()
        };
        untouched.apply_default_visibility("default");
        assert_eq!(untouched.functions[0].symbol_attrs.visibility, None);
    }

    #[test]
    fn opcode_all_lists_every_opcode_once() {
        for (i, op) in Opcode::ALL.iter().enumerate() {
            assert!(op.is_listed());
            assert!(!Opcode::ALL[..i].contains(op), "{op:?} is listed twice");
        }
    }

    /// Every float comparison opcode is exactly one `FloatCmp`, and back:
    /// a consumer that takes one apart can trust it names every opcode. The
    /// four signaling ones are the relationals, each the twin of a quiet one
    /// with the same predicate; equality is quiet only.
    #[test]
    fn float_cmp_names_every_float_comparison_once() {
        let mut signaling = 0;
        for &op in Opcode::ALL {
            let Some(cmp) = op.float_cmp() else {
                assert!(!op.is_float_comparison(), "{op:?}");
                continue;
            };
            assert_eq!(Opcode::from(cmp), op, "{cmp:?}");
            assert_eq!(cmp.quiet().nan(), NanCompare::Quiet, "{cmp:?}");
            assert_eq!(cmp.quiet().quiet(), cmp.quiet(), "{cmp:?}");
            match cmp.signaling() {
                Some(s) => {
                    assert_eq!(s.nan(), NanCompare::Signaling, "{cmp:?}");
                    assert_eq!(s.quiet(), cmp.quiet(), "{cmp:?}");
                }
                None => assert!(matches!(cmp, FloatCmp::Eq | FloatCmp::Ne)),
            }
            if cmp.nan() == NanCompare::Signaling {
                signaling += 1;
                assert_ne!(Opcode::from(cmp.quiet()), op);
            }
        }
        assert_eq!(signaling, 4);
    }

    /// What may raise a floating-point exception, by opcode: arithmetic and
    /// signaling comparisons may, the exact bit operations and quiet
    /// comparisons do not, and every non-floating opcode but a call or an
    /// `asm` raises nothing.
    #[test]
    fn fp_raise_separates_exact_float_operations_from_rounding_ones() {
        use Opcode::*;
        for op in [
            FAdd, FSub, FMul, FDiv, Sqrt, Fma, FMin, FMax, FCmpsOLt, FCmpsOGe, Call, Asm,
        ] {
            assert_eq!(op.fp_raise(), FpRaise::May, "{op:?}");
        }
        for op in [
            FNeg, Fabs, CopySign, Signbit, FCmpOEq, FCmpONe, FCmpOLt, FCmpOGe, Add, Mul, DivS,
            Load, Select, Trunc, Sext,
        ] {
            assert_eq!(op.fp_raise(), FpRaise::Never, "{op:?}");
        }
        for op in [FCvtF, FCvtS, FCvtU, SCvtF, UCvtF] {
            assert_eq!(op.fp_raise(), FpRaise::Conversion, "{op:?}");
        }
        for &op in Opcode::ALL {
            if let Some(cmp) = op.float_cmp() {
                let signals = cmp.nan() == NanCompare::Signaling;
                assert_eq!(op.fp_raise() == FpRaise::May, signals, "{op:?}");
            }
        }
        assert_eq!(Simd(SimdOp::FMul).fp_raise(), FpRaise::May);
        assert_eq!(Simd(SimdOp::FNeg).fp_raise(), FpRaise::Never);
        assert_eq!(Simd(SimdOp::Add).fp_raise(), FpRaise::Never);
    }

    /// A conversion raises exactly where the destination does not hold
    /// every source value: widening and small integers are exact,
    /// narrowing, floating to integer and wide integers are not, and
    /// `_Bool` compares quietly.
    #[test]
    fn conversion_raises_fp_where_the_destination_loses_values() {
        let x86 = TypeTable::new(&Target::new(Arch::X86_64, crate::target::Os::Linux));
        let a64 = TypeTable::new(&Target::new(Arch::Aarch64, crate::target::Os::Linux));
        let mac = TypeTable::new(&Target::new(Arch::Aarch64, crate::target::Os::MacOS));
        for t in [&x86, &a64, &mac] {
            let cases = [
                (t.float_id, t.double_id, false),
                (t.double_id, t.longdouble_id, false),
                (t.float16_id, t.float_id, false),
                (t.double_id, t.float_id, true),
                (t.float_id, t.float16_id, true),
                (t.double_id, t.int_id, true),
                (t.float_id, t.ulong_id, true),
                (t.double_id, t.bool_id, false),
                (t.bool_id, t.float_id, false),
                (t.short_id, t.float_id, false),
                (t.int_id, t.double_id, false),
                (t.uint_id, t.double_id, false),
                (t.int_id, t.float_id, true),
                (t.long_id, t.double_id, true),
                (t.int_id, t.long_id, false),
                (t.double_id, t.void_id, false),
                (
                    t.make_complex(t.float_id),
                    t.make_complex(t.double_id),
                    false,
                ),
                (
                    t.make_complex(t.double_id),
                    t.make_complex(t.float_id),
                    true,
                ),
                (t.double_id, t.make_complex(t.float_id), true),
                (t.make_complex(t.double_id), t.double_id, false),
            ];
            for (from, to, raises) in cases {
                assert_eq!(
                    conversion_raises_fp(t, from, to),
                    raises,
                    "{:?} -> {:?}",
                    t.get(from).kind,
                    t.get(to).kind
                );
            }
        }
        // `long double` decides by its format: x87 holds every `long`,
        // binary64 (Apple arm64) does not, and nothing holds a `double`
        // narrowed back from one.
        assert!(!conversion_raises_fp(&x86, x86.ulong_id, x86.longdouble_id));
        assert!(!conversion_raises_fp(&a64, a64.long_id, a64.longdouble_id));
        assert!(conversion_raises_fp(&mac, mac.long_id, mac.longdouble_id));
        assert!(!conversion_raises_fp(
            &mac,
            mac.longdouble_id,
            mac.double_id
        ));
        assert!(conversion_raises_fp(&x86, x86.longdouble_id, x86.double_id));
        assert!(conversion_raises_fp(&a64, a64.int128_id, a64.float128_id));

        // The instruction asks the same question of the types it records.
        let mut narrow =
            Instruction::unop(Opcode::FCvtF, PseudoId(1), PseudoId(0), x86.float_id, 32);
        narrow.src_typ = Some(x86.double_id);
        narrow.src_size = 64;
        assert!(narrow.may_raise_fp_exception(&x86));
        let mut widen =
            Instruction::unop(Opcode::FCvtF, PseudoId(1), PseudoId(0), x86.double_id, 64);
        widen.src_typ = Some(x86.float_id);
        widen.src_size = 32;
        assert!(!widen.may_raise_fp_exception(&x86));
        widen.src_typ = None;
        assert!(
            widen.may_raise_fp_exception(&x86),
            "no source type: assume it raises"
        );
    }

    /// I5 -- a memory access is a DCE root, except a `Load`. Over the whole
    /// table, so an opcode added in breach of it fails here.
    #[test]
    fn memory_access_is_a_side_effect_except_load() {
        for &op in Opcode::ALL {
            if op.may_access_memory() && op != Opcode::Load {
                assert!(
                    op.has_side_effects(),
                    "{op:?} reaches memory but DCE may delete it"
                );
            }
        }
        assert!(Opcode::Load.may_access_memory());
        assert!(!Opcode::Load.has_side_effects());
        // The converse does not hold: a branch is a root and touches no memory.
        assert!(Opcode::Br.has_side_effects());
        assert!(!Opcode::Br.may_access_memory());
    }

    /// I2 -- a memory barrier is a DCE root, or DCE could drop a `Fence`, an
    /// atomic, a call or an `asm("" ::: "memory")` whose result is unused.
    /// Every opcode is tried bare and carrying a `"memory"` clobber, which
    /// only `Asm` reads.
    #[test]
    fn memory_barrier_is_a_side_effect() {
        let mut barriers = 0;
        for &op in Opcode::ALL {
            let mut clobbering = Instruction::new(op);
            clobbering.extra_mut().asm_data = Some(Box::new(AsmData {
                template: String::new(),
                outputs: Vec::new(),
                inputs: Vec::new(),
                clobbers: vec!["memory".to_string()],
                goto_labels: Vec::new(),
            }));
            for insn in [Instruction::new(op), clobbering] {
                if insn.is_memory_barrier() {
                    barriers += 1;
                    assert!(op.has_side_effects(), "{op:?} is a barrier DCE may delete");
                }
            }
        }
        // Fence, Call, Setjmp, Longjmp and the atomics, each twice, and Asm once.
        assert_eq!(barriers, 2 * 13 + 1);
    }

    /// A vector operation computes in registers: it touches no memory, so
    /// nothing orders around it and dead-code elimination may delete it.
    #[test]
    fn test_simd_opcodes_are_pure() {
        for op in SimdOp::ALL {
            let op = Opcode::Simd(op);
            assert!(!op.may_access_memory() && !op.has_side_effects(), "{op:?}");
            assert!(!op.is_terminator() && !op.is_comparison(), "{op:?}");
        }
    }

    #[test]
    fn test_opcode_is_terminator() {
        assert!(Opcode::Ret.is_terminator());
        assert!(Opcode::Br.is_terminator());
        assert!(Opcode::Cbr.is_terminator());
        assert!(!Opcode::Add.is_terminator());
        assert!(!Opcode::Load.is_terminator());
    }

    /// Every op a backend addresses as `src[0] + offset` has a displacement:
    /// the atomics as well as plain loads and stores.
    #[test]
    fn every_memory_access_has_a_displacement() {
        for op in [
            Opcode::Load,
            Opcode::Store,
            Opcode::AtomicLoad,
            Opcode::AtomicStore,
            Opcode::AtomicSwap,
            Opcode::AtomicCas,
            Opcode::AtomicFetchAdd,
            Opcode::AtomicFetchSub,
            Opcode::AtomicFetchAnd,
            Opcode::AtomicFetchOr,
            Opcode::AtomicFetchXor,
        ] {
            let insn = Instruction::new(op).with_src(PseudoId(0)).with_offset(-8);
            assert_eq!(insn.displacement(), -8, "{op:?}");
        }
    }

    #[test]
    fn test_pseudo_display() {
        let reg = Pseudo::reg(PseudoId(1), 1);
        assert_eq!(format!("{}", reg), "%r1");

        let reg_named = Pseudo::reg(PseudoId(2), 2).with_name("x");
        assert_eq!(format!("{}", reg_named), "%r2(x)");

        let val = Pseudo::val(PseudoId(3), 42);
        assert_eq!(format!("{}", val), "$42");

        let big_val = Pseudo::val(PseudoId(4), 0x1000);
        assert_eq!(format!("{}", big_val), "$0x1000");

        let arg = Pseudo::arg(PseudoId(5), 0);
        assert_eq!(format!("{}", arg), "%arg0");
    }

    #[test]
    fn test_instruction_binop() {
        let types = TypeTable::new(&Target::host());
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        assert_eq!(insn.op, Opcode::Add);
        assert_eq!(insn.target, Some(PseudoId(3)));
        assert_eq!(insn.src.len(), 2);
    }

    #[test]
    fn test_instruction_display() {
        let types = TypeTable::new(&Target::host());
        let insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        let ctx = IrCtx {
            types: &types,
            func: None,
        };
        let s = format!("{}", insn.display(ctx));
        assert!(s.contains("add"));
        assert!(s.contains("%3"));
        assert!(s.contains("%1"));
        assert!(s.contains("%2"));
    }

    #[test]
    fn test_basic_block() {
        let mut bb = BasicBlock::new(BasicBlockId(0));
        assert!(!bb.is_terminated());

        bb.add_insn(Instruction::new(Opcode::Nop));
        assert!(!bb.is_terminated());

        bb.add_insn(Instruction::ret(None));
        assert!(bb.is_terminated());
    }

    /// The dump shows what the IR actually holds.
    ///
    /// Everything asserted here was absent before the printer took the tables:
    /// a `SetVal`'s constant lives in its target pseudo, so `return 42;`
    /// printed as `%0 = setval.32` with the 42 nowhere on the line; types
    /// printed as `type#7`; and a symbol operand printed as a bare index.
    /// `ir/README.md` documented the `$20` operand form the whole time.
    #[test]
    fn test_display_resolves_constants_and_types() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("f", types.int_id);

        // A constant pseudo, as `SetVal` builds one: the value is in the
        // *target*, not in the instruction.
        let k = PseudoId(0);
        func.add_pseudo(Pseudo::val(k, 42));

        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::set_val(k, types.int_id, 32));
        entry.add_insn(Instruction::ret(Some(k)));
        func.add_block(entry);

        let s = format!("{}", func.display(&types));

        // The constant reaches the line it defines...
        assert!(
            s.contains("setval.32 $42"),
            "the constant must be printed: {s}"
        );
        // ...and a use keeps both the id and the value.
        assert!(s.contains("%0($42)"), "a constant operand shows both: {s}");
        // The return type is named, not indexed.
        assert!(s.contains("define int f("), "types must be named: {s}");
        assert!(!s.contains("type#"), "no raw TypeId should survive: {s}");
    }

    #[test]
    fn test_function_display() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("main", types.int_id);
        func.add_param("argc", types.int_id);

        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.add_insn(Instruction::ret(Some(PseudoId(0))));
        func.add_block(entry);

        let s = format!("{}", func.display(&types));
        assert!(s.contains("define"));
        assert!(s.contains("main"));
        assert!(s.contains("argc"));
        assert!(s.contains("ret"));
    }

    #[test]
    fn test_branch_instruction() {
        let br = Instruction::br(BasicBlockId(1));
        assert_eq!(br.op, Opcode::Br);
        assert_eq!(br.bb_true, Some(BasicBlockId(1)));

        let cbr = Instruction::cbr(PseudoId(0), BasicBlockId(1), BasicBlockId(2));
        assert_eq!(cbr.op, Opcode::Cbr);
        assert_eq!(cbr.src.len(), 1);
        assert_eq!(cbr.bb_true, Some(BasicBlockId(1)));
        assert_eq!(cbr.bb_false, Some(BasicBlockId(2)));
    }

    #[test]
    fn test_call_instruction() {
        let mut types = TypeTable::new(&Target::host());
        let char_ptr = types.intern(Type::pointer(types.char_id));
        let arg_types = vec![char_ptr, types.int_id];
        let mut call = Instruction::call(
            Some(PseudoId(1)),
            "printf",
            vec![PseudoId(2), PseudoId(3)],
            arg_types.clone(),
            types.int_id,
            32,
        );
        // Add abi_info (required for codegen)
        call.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(
            vec![
                ArgClass::Direct {
                    classes: vec![RegClass::Integer],
                    size_bits: 64,
                },
                ArgClass::Direct {
                    classes: vec![RegClass::Integer],
                    size_bits: 32,
                },
            ],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 32,
            },
        )));
        assert_eq!(call.op, Opcode::Call);
        assert_eq!(call.extra().func_name, Some("printf".to_string()));
        assert_eq!(call.src.len(), 2);
        assert_eq!(call.extra().arg_types.len(), 2);
        assert!(call.extra().abi_info.is_some());
    }

    #[test]
    fn test_load_store() {
        let types = TypeTable::new(&Target::host());

        let load = Instruction::load(PseudoId(1), PseudoId(2), 8, types.int_id, 32);
        assert_eq!(load.op, Opcode::Load);
        assert_eq!(load.offset, 8);

        let store = Instruction::store(PseudoId(1), PseudoId(2), 0, types.int_id, 32);
        assert_eq!(store.op, Opcode::Store);
        assert_eq!(store.src.len(), 2);
    }

    #[test]
    fn test_module() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();

        module.add_global("counter", types.int_id, Initializer::Int(0));

        let func = Function::new("main", types.int_id);
        module.add_function(func);

        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.functions.len(), 1);
    }

    #[test]
    fn test_module_extern_symbols() {
        let mut module = Module::default();

        // New module should have empty extern_symbols
        assert!(module.extern_symbols.is_empty());

        // Can insert extern symbols
        module.extern_symbols.insert("printf".to_string());
        module.extern_symbols.insert("malloc".to_string());

        assert_eq!(module.extern_symbols.len(), 2);
        assert!(module.extern_symbols.contains("printf"));
        assert!(module.extern_symbols.contains("malloc"));

        // Can remove symbols (simulates defining them after extern declaration)
        module.extern_symbols.remove("printf");
        assert_eq!(module.extern_symbols.len(), 1);
        assert!(!module.extern_symbols.contains("printf"));
        assert!(module.extern_symbols.contains("malloc"));
    }

    #[test]
    fn test_memory_order_display() {
        assert_eq!(format!("{}", MemoryOrder::Relaxed), "relaxed");
        assert_eq!(format!("{}", MemoryOrder::Consume), "consume");
        assert_eq!(format!("{}", MemoryOrder::Acquire), "acquire");
        assert_eq!(format!("{}", MemoryOrder::Release), "release");
        assert_eq!(format!("{}", MemoryOrder::AcqRel), "acq_rel");
        assert_eq!(format!("{}", MemoryOrder::SeqCst), "seq_cst");
    }

    #[test]
    fn test_memory_order_halves() {
        use MemoryOrder::*;
        let all = [Relaxed, Consume, Acquire, Release, AcqRel, SeqCst];
        for (value, order) in all.into_iter().enumerate() {
            assert_eq!(MemoryOrder::from_value(value as i128), Some(order));
        }
        assert_eq!(MemoryOrder::from_value(6), None);
        assert_eq!(MemoryOrder::from_value(-1), None);
        let acquiring: Vec<_> = all.into_iter().filter(|o| o.acquires()).collect();
        assert_eq!(acquiring, [Consume, Acquire, AcqRel, SeqCst]);
        let releasing: Vec<_> = all.into_iter().filter(|o| o.releases()).collect();
        assert_eq!(releasing, [Release, AcqRel, SeqCst]);
    }

    #[test]
    fn test_memory_order_default() {
        let order: MemoryOrder = Default::default();
        assert_eq!(order, MemoryOrder::Relaxed);
    }

    #[test]
    fn test_atomic_opcodes_not_terminators() {
        // Atomic operations should not be terminators
        assert!(!Opcode::AtomicLoad.is_terminator());
        assert!(!Opcode::AtomicStore.is_terminator());
        assert!(!Opcode::AtomicSwap.is_terminator());
        assert!(!Opcode::AtomicCas.is_terminator());
        assert!(!Opcode::AtomicFetchAdd.is_terminator());
        assert!(!Opcode::AtomicFetchSub.is_terminator());
        assert!(!Opcode::AtomicFetchAnd.is_terminator());
        assert!(!Opcode::AtomicFetchOr.is_terminator());
        assert!(!Opcode::AtomicFetchXor.is_terminator());
        assert!(!Opcode::Fence.is_terminator());
    }

    #[test]
    fn test_atomic_opcode_names() {
        assert_eq!(Opcode::AtomicLoad.name(), "atomic_load");
        assert_eq!(Opcode::AtomicStore.name(), "atomic_store");
        assert_eq!(Opcode::AtomicSwap.name(), "atomic_swap");
        assert_eq!(Opcode::AtomicCas.name(), "atomic_cas");
        assert_eq!(Opcode::AtomicFetchAdd.name(), "atomic_fetch_add");
        assert_eq!(Opcode::AtomicFetchSub.name(), "atomic_fetch_sub");
        assert_eq!(Opcode::AtomicFetchAnd.name(), "atomic_fetch_and");
        assert_eq!(Opcode::AtomicFetchOr.name(), "atomic_fetch_or");
        assert_eq!(Opcode::AtomicFetchXor.name(), "atomic_fetch_xor");
        assert_eq!(Opcode::Fence.name(), "fence");
    }

    #[test]
    fn test_instruction_with_memory_order() {
        let mut insn = Instruction::new(Opcode::AtomicLoad);
        assert_eq!(insn.extra().memory_order, MemoryOrder::Relaxed); // default

        insn = insn.with_memory_order(MemoryOrder::SeqCst);
        assert_eq!(insn.extra().memory_order, MemoryOrder::SeqCst);

        insn = insn.with_memory_order(MemoryOrder::Acquire);
        assert_eq!(insn.extra().memory_order, MemoryOrder::Acquire);
    }

    /// A string initializer taken apart into its elements: one per code unit
    /// that fits, cut at the array, each at its element's offset.
    #[test]
    fn test_string_initializer_as_its_elements() {
        let s = Initializer::String("ab\u{e9}".into())
            .string_as_array(1, 6)
            .unwrap();
        let Initializer::Array { elements, .. } = s else {
            panic!("an array");
        };
        assert_eq!(
            elements,
            vec![
                (0, Initializer::Int(97)),
                (1, Initializer::Int(98)),
                (2, Initializer::Int(0xe9))
            ]
        );
        // Cut to the array, not the array stretched to the string.
        let w = Initializer::Utf16String(vec![1, 2, 3])
            .string_as_array(2, 4)
            .unwrap();
        let Initializer::Array { elements, .. } = w else {
            panic!("an array");
        };
        assert_eq!(
            elements,
            vec![(0, Initializer::Int(1)), (2, Initializer::Int(2))]
        );
        assert!(Initializer::Int(1).string_as_array(1, 1).is_none());
    }

    /// Every instruction pays for every field it has, so the ones only a few
    /// opcodes use live in `InsnExtra`. Two hundred and forty-eight bytes, one
    /// heap allocation each, when they were inline.
    #[test]
    fn test_an_instruction_keeps_its_rare_fields_out_of_line() {
        let size = std::mem::size_of::<Instruction>();
        eprintln!("size_of::<Instruction>() = {size}");
        assert!(size <= 128, "{size}");
    }

    #[test]
    fn test_local_var_is_atomic() {
        let mut types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.void_id);
        let atomic_int = types.intern(crate::types::Type::with_modifiers(
            crate::types::TypeKind::Int,
            crate::types::TypeModifiers::ATOMIC,
        ));

        // A non-atomic local
        let sym1 = PseudoId(1);
        func.add_pseudo(Pseudo::sym(sym1, "x".to_string()));
        func.add_local("x", sym1, types.int_id, None, None);

        // An atomic one: the answer comes from the type, not a flag
        let sym2 = PseudoId(2);
        func.add_pseudo(Pseudo::sym(sym2, "y".to_string()));
        func.add_local("y", sym2, atomic_int, None, None);

        assert!(func.locals.get("x").unwrap().is_ordinary(&types));
        assert!(!func.locals.get("y").unwrap().is_ordinary(&types));
    }

    #[test]
    fn test_fabs_opcode() {
        assert_eq!(Opcode::Fabs.name(), "fabs");
        assert!(!Opcode::Fabs.is_terminator());
    }

    #[test]
    fn test_sign_opcodes() {
        assert_eq!(Opcode::Signbit.name(), "signbit");
        assert_eq!(Opcode::CopySign.name(), "copysign");
        assert!(!Opcode::Signbit.is_terminator());
        assert!(!Opcode::CopySign.is_terminator());
    }

    /// `Sqrt` is an ordinary value: no terminator, and nothing but its result.
    #[test]
    fn test_sqrt_opcode() {
        assert_eq!(Opcode::Sqrt.name(), "sqrt");
        assert!(!Opcode::Sqrt.is_terminator());
        assert!(!Opcode::Sqrt.has_side_effects());
        assert!(Opcode::Sqrt.is_libm());
        assert!(!Opcode::Fabs.is_libm() && !Opcode::Call.is_libm());
    }

    /// One opcode per rounding, each named apart from the others and from
    /// the integer `trunc`.
    #[test]
    fn test_round_to_integral_opcodes() {
        use IntegralRounding::*;
        let names: Vec<&str> = [Floor, Ceil, Trunc, Round, Rint, NearbyInt]
            .into_iter()
            .map(|how| {
                let op = Opcode::RoundToIntegral(how);
                assert!(op.is_libm() && !op.is_terminator() && !op.has_side_effects());
                op.name()
            })
            .collect();
        assert_eq!(
            names,
            ["ffloor", "fceil", "ftrunc", "fround", "frint", "fnearbyint"]
        );
        assert_ne!(Opcode::RoundToIntegral(Trunc).name(), Opcode::Trunc.name());
    }

    /// `fmin`, `fmax` and `fma` are libm opcodes like the rest.
    #[test]
    fn test_min_max_fma_opcodes() {
        for (op, name) in [
            (Opcode::FMin, "fmin"),
            (Opcode::FMax, "fmax"),
            (Opcode::Fma, "fma"),
        ] {
            assert_eq!(op.name(), name);
            assert!(op.is_libm() && !op.is_terminator() && !op.has_side_effects());
        }
    }

    fn storage(is_static: bool, is_const: bool, is_thread_local: bool) -> GlobalStorage {
        GlobalStorage {
            is_static,
            is_const,
            is_thread_local,
        }
    }

    #[test]
    fn test_define_global_tentative_definition() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();
        let plain = storage(false, false, false);

        // Add a tentative definition (no initializer)
        module.define_global(&types, "x", types.int_id, Initializer::None, None, plain);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual definition - should replace the tentative one
        module.define_global(
            &types,
            "x",
            types.int_id,
            Initializer::Int(42),
            Some(4),
            plain,
        );
        assert_eq!(module.globals.len(), 1); // Still only one global
        assert!(matches!(module.globals[0].init, Initializer::Int(42)));
        assert_eq!(module.globals[0].explicit_align, Some(4));
    }

    #[test]
    fn test_define_global_non_tentative_not_replaced() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();
        let plain = storage(false, false, false);

        // Add a real definition (with initializer)
        module.define_global(&types, "x", types.int_id, Initializer::Int(10), None, plain);
        assert_eq!(module.globals.len(), 1);

        // Add another definition with same name - should NOT replace (adds new entry)
        module.define_global(&types, "x", types.int_id, Initializer::Int(20), None, plain);
        assert_eq!(module.globals.len(), 2); // Two globals now (linker will error)
    }

    #[test]
    fn test_define_global_tls_tentative_definition() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();
        let tls = storage(false, false, true);

        // Add a TLS tentative definition
        module.define_global(
            &types,
            "tls_var",
            types.int_id,
            Initializer::None,
            None,
            tls,
        );
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::None));

        // Add actual TLS definition - should replace
        let init = Initializer::Int(100);
        module.define_global(&types, "tls_var", types.int_id, init, Some(8), tls);
        assert_eq!(module.globals.len(), 1);
        assert!(matches!(module.globals[0].init, Initializer::Int(100)));
        assert!(module.globals[0].is_thread_local);
        assert_eq!(module.globals[0].explicit_align, Some(8));
    }

    /// Each of the eight storages reaches the definition as given.
    #[test]
    fn test_define_global_every_storage() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();
        let mut all = Vec::new();
        for bits in 0..8u8 {
            let st = storage(bits & 1 != 0, bits & 2 != 0, bits & 4 != 0);
            let name = format!("g{bits}");
            module.define_global(&types, &name, types.int_id, Initializer::Int(1), None, st);
            all.push((name, st));
        }
        assert_eq!(module.globals.len(), 8);
        for (g, (name, st)) in module.globals.iter().zip(&all) {
            assert_eq!(&g.name, name);
            let got = storage(g.is_static, g.is_const, g.is_thread_local);
            assert_eq!(got, *st, "{name}");
        }
    }

    /// Completing a tentative definition takes the definition's linkage,
    /// keeps `const` and thread-local storage from either declaration, and
    /// keeps the tentative one's alignment when the definition has none.
    #[test]
    fn test_define_global_completion_merges_storage() {
        let types = TypeTable::new(&Target::host());
        let mut module = Module::default();
        let int = types.int_id;
        module.define_global(
            &types,
            "a",
            int,
            Initializer::None,
            Some(16),
            storage(true, true, true),
        );
        module.define_global(
            &types,
            "a",
            int,
            Initializer::Int(1),
            None,
            storage(false, false, false),
        );
        let a = &module.globals[0];
        assert_eq!(
            storage(a.is_static, a.is_const, a.is_thread_local),
            storage(false, true, true)
        );
        assert_eq!(a.explicit_align, Some(16));

        module.define_global(
            &types,
            "b",
            int,
            Initializer::None,
            None,
            storage(false, false, false),
        );
        module.define_global(
            &types,
            "b",
            int,
            Initializer::Int(2),
            None,
            storage(true, true, true),
        );
        let b = &module.globals[1];
        assert_eq!(
            storage(b.is_static, b.is_const, b.is_thread_local),
            storage(true, true, true)
        );
        assert_eq!(module.globals.len(), 2);
    }

    /// One label per distinct contents, from the index and from a linear
    /// search alike, including for literals a pass appended directly.
    #[test]
    fn test_add_string_dedupes() {
        let mut module = Module::default();
        let a = module.add_string("a".to_string());
        let b = module.add_string("b".to_string());
        assert_ne!(a, b);
        assert_eq!(module.add_string("a".to_string()), a);
        assert_eq!(module.add_string("b".to_string()), b);
        assert_eq!(module.strings.len(), 2);

        // A literal appended behind `add_string`'s back, as the libcall
        // folds do, is found too.
        let c = string_label(module.strings.len());
        module.strings.push((c.clone(), "c".to_string()));
        assert_eq!(module.add_string("c".to_string()), c);
        let d = module.add_string("d".to_string());
        assert_eq!(module.strings.len(), 4);
        for (label, content) in module.strings.clone() {
            let first = module.strings.iter().find(|(_, s)| *s == content).unwrap();
            assert_eq!(first.0, label);
            assert_eq!(module.add_string(content), label);
        }
        assert_eq!(module.strings.len(), 4);
        assert_eq!(module.strings[3].0, d);

        // A list replaced wholesale is reindexed, not answered from stale
        // positions.
        module.strings = vec![(string_label(0), "d".to_string())];
        assert_eq!(module.add_string("d".to_string()), string_label(0));
        assert_eq!(module.add_string("a".to_string()), string_label(1));
        assert_eq!(module.strings.len(), 2);
    }

    #[test]
    fn test_switch_insn_uses_src() {
        let insn = Instruction::switch_insn(
            PseudoId(5),
            vec![(0, 0, BasicBlockId(1)), (1, 1, BasicBlockId(2))],
            Some(BasicBlockId(3)),
            32,
        );
        assert!(insn.target.is_none());
        assert_eq!(insn.src.len(), 1);
        assert_eq!(insn.src[0], PseudoId(5));
        assert_eq!(insn.extra().switch_cases.len(), 2);
        assert_eq!(insn.extra().switch_default, Some(BasicBlockId(3)));
        let types = TypeTable::new(&Target::host());
        let ctx = IrCtx {
            types: &types,
            func: None,
        };
        let s = format!("{}", insn.display(ctx));
        assert!(s.contains("switch"));
        assert!(s.contains("%5"));
        assert!(s.contains("default"));
    }

    #[test]
    fn test_instruction_kill() {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::binop(
            Opcode::Add,
            PseudoId(3),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        );
        insn.phi_list.push((BasicBlockId(0), PseudoId(10)));
        assert_eq!(insn.op, Opcode::Add);
        assert!(insn.target.is_some());
        assert!(!insn.src.is_empty());
        assert!(!insn.phi_list.is_empty());
        insn.kill();
        assert_eq!(insn.op, Opcode::Nop);
        assert!(insn.target.is_none());
        assert!(insn.src.is_empty());
        assert!(insn.phi_list.is_empty());
    }

    #[test]
    fn test_function_const_val() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut func = Function::new("test_const_val", types.int_id);
        let val_id = func.create_const_pseudo(42);
        assert_eq!(func.const_val(val_id), Some(42));
        let reg_id = func.alloc_pseudo();
        let reg = Pseudo::reg(reg_id, reg_id.0);
        func.add_pseudo(reg);
        assert_eq!(func.const_val(reg_id), None);
        assert_eq!(func.const_val(PseudoId(9999)), None);
    }

    #[test]
    fn test_function_sym_name_of() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut func = Function::new("test_sym_name", types.int_id);
        let sym_id = func.alloc_pseudo();
        let sym = Pseudo::sym(sym_id, "my_symbol".to_string());
        func.add_pseudo(sym);
        assert_eq!(func.sym_name_of(sym_id), Some("my_symbol"));
        let reg_id = func.alloc_pseudo();
        let reg = Pseudo::reg(reg_id, reg_id.0);
        func.add_pseudo(reg);
        assert_eq!(func.sym_name_of(reg_id), None);
        assert_eq!(func.sym_name_of(PseudoId(9999)), None);
    }

    /// A local's `Sym` carries the local's name, which a global may share;
    /// only the global's `Sym` names a global.
    #[test]
    fn test_function_global_sym_name_asks_identity() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut func = Function::new("f", types.int_id);
        let global = func.alloc_pseudo();
        func.add_pseudo(Pseudo::sym(global, "count".to_string()));
        let local = func.alloc_pseudo();
        func.add_pseudo(Pseudo::sym(local, "count".to_string()));
        func.add_local("count", local, types.int_id, None, None);
        let reg = func.alloc_pseudo();
        func.add_pseudo(Pseudo::reg(reg, reg.0));

        assert_eq!(func.global_sym_name(global), Some("count"));
        assert_eq!(func.global_sym_name(local), None, "the parameter");
        assert_eq!(func.sym_name_of(local), Some("count"));
        assert_eq!(func.global_sym_name(reg), None);
    }

    /// The global index follows `globals` however it grew: through the
    /// module, pushed to directly, or -- which nothing does, but which must
    /// still not be answered wrongly -- reordered.
    #[test]
    fn global_index_finds_every_global_by_name() {
        let types = TypeTable::new(&Target::host());
        let mut m = Module::default();
        m.add_global("a", types.int_id, Initializer::None);
        m.globals
            .push(GlobalDef::new("b", types.int_id, Initializer::Int(2)));
        assert_eq!(
            m.global_mut("b").map(|g| g.init.clone()),
            Some(Initializer::Int(2))
        );
        assert!(m.global_mut("a").is_some());
        assert!(m.global_mut("c").is_none());
        m.globals
            .push(GlobalDef::new("c", types.int_id, Initializer::Int(3)));
        assert!(m.global_mut("c").is_some(), "an append after a lookup");
        m.globals.swap(0, 2);
        assert_eq!(
            m.global_mut("a").map(|g| g.name.clone()),
            Some("a".to_string())
        );
        assert_eq!(
            m.global_mut("c").map(|g| g.name.clone()),
            Some("c".to_string())
        );
    }

    /// An `Arg` holds a value of its parameter's type only for a scalar, and
    /// the hidden sret pointer shifts which parameter each `Arg` is.
    #[test]
    fn test_arg_value_type() {
        let types = TypeTable::new(&Target::host());
        for sret in [false, true] {
            let mut f = Function::new("f", types.void_id);
            let off = u32::from(sret);
            if sret {
                f.add_pseudo(Pseudo::arg(PseudoId(9), 0).with_name("__sret"));
                f.sret = Some(PseudoId(9));
            }
            let params = [
                types.char_id,
                types.complex_double_id,
                types.pointer_to(types.int_id),
            ];
            for (i, t) in params.iter().enumerate() {
                f.add_param(format!("p{i}"), *t);
                f.add_pseudo(Pseudo::arg(PseudoId(i as u32), i as u32 + off));
            }
            assert_eq!(f.sret_arg().is_some(), sret);
            assert_eq!(f.arg_value_type(PseudoId(0), &types), Some(types.char_id));
            assert_eq!(f.arg_value_type(PseudoId(1), &types), None, "complex");
            assert_eq!(f.arg_value_type(PseudoId(2), &types), Some(params[2]));
            if sret {
                assert_eq!(f.arg_value_type(PseudoId(9), &types), None, "sret");
            }
            // The view a loop over every argument uses gives the same answers.
            let args = f.arg_types();
            assert_eq!(args.sret, f.sret_arg());
            for arg in 0..5 {
                assert_eq!(args.of(arg), f.param_type_of_arg(arg), "Arg({arg})");
            }
            assert_eq!(args.of(off), Some(types.char_id));
            for (i, t) in params.iter().enumerate() {
                assert_eq!(args.of(args.arg_of_param(i)), Some(*t));
            }
        }
    }

    /// Only a `PhiSource`'s `phi_list` is a back-pointer; a phi's is its
    /// incoming pairs.
    #[test]
    fn test_phi_source_dest() {
        let types = TypeTable::new(&Target::host());
        let mut src = Instruction::phi_source(PseudoId(1), PseudoId(0), types.int_id, 32);
        assert_eq!(src.phi_source_dest(), None, "no back-pointer yet");
        src.phi_list = vec![(BasicBlockId(3), PseudoId(2))];
        assert_eq!(src.phi_source_dest(), Some((BasicBlockId(3), PseudoId(2))));
        let mut phi = Instruction::phi(PseudoId(2), types.int_id, 32);
        phi.phi_list = vec![(BasicBlockId(1), PseudoId(1))];
        assert_eq!(phi.phi_source_dest(), None, "a phi feeds nothing");
    }

    /// Every asm output across the function, and nothing else.
    #[test]
    fn test_asm_defined_pseudos() {
        let types = TypeTable::new(&Target::host());
        let asm = |outs: &[u32]| {
            Instruction::asm(AsmData {
                template: String::new(),
                outputs: outs
                    .iter()
                    .map(|&n| AsmConstraint::new(PseudoId(n), "=r", Arch::X86_64, 32))
                    .collect(),
                inputs: vec![AsmConstraint::new(PseudoId(9), "r", Arch::X86_64, 32)],
                clobbers: vec![],
                goto_labels: vec![],
            })
        };
        let mut f = Function::new("f", types.void_id);
        assert!(f.asm_defined_pseudos().is_empty());
        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(asm(&[1, 2]));
        b0.add_insn(Instruction::unop(
            Opcode::Copy,
            PseudoId(3),
            PseudoId(1),
            types.int_id,
            32,
        ));
        let mut b1 = BasicBlock::new(BasicBlockId(1));
        b1.add_insn(asm(&[4]));
        f.add_block(b0);
        f.add_block(b1);
        let got = f.asm_defined_pseudos();
        let want: HashSet<PseudoId> = [1, 2, 4].into_iter().map(PseudoId).collect();
        assert_eq!(got, want);
    }

    #[test]
    fn test_returns_via_sret() {
        let mut insn = Instruction::new(Opcode::Call);
        assert!(!insn.returns_via_sret());
        insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            },
        )));
        assert!(!insn.returns_via_sret());
        insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Indirect {
                align: 8,
                size_bytes: 32,
            },
        )));
        assert!(insn.returns_via_sret());
    }

    #[test]
    fn test_returns_two_regs() {
        let mut insn = Instruction::new(Opcode::Ret);
        assert!(!insn.returns_two_regs());
        insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer],
                size_bits: 64,
            },
        )));
        assert!(!insn.returns_two_regs());
        insn.extra_mut().abi_info = Some(Box::new(CallAbiInfo::new(
            vec![],
            ArgClass::Direct {
                classes: vec![RegClass::Integer, RegClass::Integer],
                size_bits: 128,
            },
        )));
        assert!(insn.returns_two_regs());
    }

    /// The three classes whose `Ret` hands back an aggregate's *address*, and
    /// the size bound that is part of the rule.
    ///
    /// `Direct { classes: [Sse] }` is the discriminating row: at sixteen bytes
    /// it is one SSE register holding a whole `__float128`, so the `Ret` names
    /// the storage; at eight it is `struct { float a, b; }`, which comes back
    /// *as* a value and never reaches `emit_reg_aggregate_return` at all. Answering
    /// the first one "no" is what made the inliner phi an address as though it
    /// were the aggregate.
    #[test]
    fn test_aggregate_ret_is_address() {
        let sse = |n: usize, bits: u32| ArgClass::Direct {
            classes: vec![RegClass::Sse; n],
            size_bits: bits,
        };
        assert!(aggregate_ret_is_address(&sse(1, 128), 128), "one SSE, 16B");
        assert!(!aggregate_ret_is_address(&sse(1, 64), 64), "one SSE, 8B");
        assert!(
            !aggregate_ret_is_address(&sse(2, 128), 128),
            "two SSE registers carry the halves, not an address"
        );
        assert!(
            !aggregate_ret_is_address(
                &ArgClass::Direct {
                    classes: vec![RegClass::Integer, RegClass::Integer],
                    size_bits: 128,
                },
                128
            ),
            "__int128 comes back in RAX/RDX"
        );
        assert!(aggregate_ret_is_address(
            &ArgClass::X87 { size_bits: 80 },
            128
        ));
        assert!(aggregate_ret_is_address(
            &ArgClass::Hfa {
                base: crate::abi::HfaBase::Float64,
                count: 4,
            },
            256
        ));
        assert!(
            !aggregate_ret_is_address(
                &ArgClass::Indirect {
                    align: 8,
                    size_bytes: 64,
                },
                512
            ),
            "the hidden pointer is not this"
        );
        assert!(!aggregate_ret_is_address(&ArgClass::Ignore, 0));
    }

    // Function::create_reg_pseudo

    #[test]
    fn test_create_reg_pseudo() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        func.next_pseudo = 10;

        let id1 = func.create_reg_pseudo();
        assert_eq!(id1, PseudoId(10));
        assert_eq!(func.next_pseudo, 11);

        // Verify the pseudo was registered
        let pseudo = func.get_pseudo(id1).expect("pseudo must exist");
        assert!(matches!(pseudo.kind, PseudoKind::Reg(_)));

        let id2 = func.create_reg_pseudo();
        assert_eq!(id2, PseudoId(11));
        assert_eq!(func.next_pseudo, 12);

        // IDs must be distinct
        assert_ne!(id1, id2);
    }

    // Instruction::call_with_abi

    #[test]
    fn test_call_with_abi_basic() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        let insn = Instruction::call_with_abi(
            Some(PseudoId(2)),
            "__divti3",
            vec![PseudoId(0), PseudoId(1)],
            vec![types.int128_id, types.int128_id],
            types.int128_id,
            CallingConv::C,
            &types,
            &target,
        );

        assert_eq!(insn.op, Opcode::Call);
        assert_eq!(insn.target, Some(PseudoId(2)));
        assert_eq!(insn.extra().func_name.as_deref(), Some("__divti3"));
        assert_eq!(insn.src.len(), 2);
        assert_eq!(insn.extra().arg_types.len(), 2);
        assert!(insn.extra().abi_info.is_some());

        let abi = insn.extra().abi_info.as_ref().unwrap();
        assert_eq!(abi.params.len(), 2);
    }

    #[test]
    fn test_call_with_abi_conversion() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        // float → signed int128 (__fixsfti)
        let insn = Instruction::call_with_abi(
            Some(PseudoId(1)),
            "__fixsfti",
            vec![PseudoId(0)],
            vec![types.float_id],
            types.int128_id,
            CallingConv::C,
            &types,
            &target,
        );

        assert_eq!(insn.op, Opcode::Call);
        assert_eq!(insn.extra().func_name.as_deref(), Some("__fixsfti"));
        assert!(insn.extra().abi_info.is_some());
        assert_eq!(insn.extra().abi_info.as_ref().unwrap().params.len(), 1);
    }

    #[test]
    fn test_call_with_abi_sets_size() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        let insn = Instruction::call_with_abi(
            Some(PseudoId(1)),
            "__addtf3",
            vec![PseudoId(0)],
            vec![types.double_id],
            types.double_id,
            CallingConv::C,
            &types,
            &target,
        );

        // Size should be set from ret_type
        assert_eq!(insn.size, types.size_bits(types.double_id));
    }

    // is_memory_barrier

    fn make_asm_with_clobbers(clobbers: Vec<&str>) -> Instruction {
        let mut insn = Instruction::new(Opcode::Asm);
        insn.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: clobbers.into_iter().map(String::from).collect(),
            goto_labels: Vec::new(),
        }));
        insn
    }

    #[test]
    fn test_is_memory_barrier_pure_ops_are_not_barriers() {
        for op in [
            Opcode::Add,
            Opcode::Sub,
            Opcode::Mul,
            Opcode::And,
            Opcode::Or,
            Opcode::Xor,
            Opcode::Shl,
            Opcode::Lsr,
            Opcode::Asr,
            Opcode::Neg,
            Opcode::Not,
            Opcode::SetEq,
            Opcode::Copy,
            Opcode::Load,
            Opcode::Store,
            Opcode::Br,
            Opcode::Ret,
            Opcode::Nop,
        ] {
            let insn = Instruction::new(op);
            assert!(
                !insn.is_memory_barrier(),
                "{op:?} should not be a memory barrier"
            );
        }
    }

    #[test]
    fn test_is_memory_barrier_fence_and_atomics() {
        for op in [
            Opcode::Fence,
            Opcode::AtomicLoad,
            Opcode::AtomicStore,
            Opcode::AtomicSwap,
            Opcode::AtomicCas,
            Opcode::AtomicFetchAdd,
            Opcode::AtomicFetchSub,
            Opcode::AtomicFetchAnd,
            Opcode::AtomicFetchOr,
            Opcode::AtomicFetchXor,
        ] {
            let insn = Instruction::new(op);
            assert!(
                insn.is_memory_barrier(),
                "{op:?} should be a memory barrier"
            );
        }
    }

    /// A signal fence emits no instruction, but stays the compiler barrier
    /// a thread fence is: a barrier, a side-effecting root, a memory access.
    #[test]
    fn test_signal_fence_is_a_barrier_without_an_instruction() {
        for scope in [FenceScope::Thread, FenceScope::Signal] {
            let mut insn = Instruction::new(Opcode::Fence).with_memory_order(MemoryOrder::SeqCst);
            insn.extra_mut().fence_scope = scope;
            assert!(insn.is_memory_barrier(), "{scope:?}");
            assert!(insn.op.has_side_effects(), "{scope:?}");
            assert!(insn.op.may_access_memory(), "{scope:?}");
            let hardware = (scope == FenceScope::Thread).then_some(MemoryOrder::SeqCst);
            assert_eq!(insn.hardware_fence_order(), hardware);
        }
    }

    #[test]
    fn test_is_memory_barrier_call_and_jmp() {
        // Call is always a barrier (no escape analysis in c17).
        assert!(Instruction::new(Opcode::Call).is_memory_barrier());
        // setjmp/longjmp save/restore arbitrary execution context.
        assert!(Instruction::new(Opcode::Setjmp).is_memory_barrier());
        assert!(Instruction::new(Opcode::Longjmp).is_memory_barrier());
    }

    fn constraint(c: &str, matching_output: Option<usize>) -> AsmConstraint {
        AsmConstraint {
            matching_output,
            ..AsmConstraint::new(PseudoId(0), c, crate::target::Arch::X86_64, 64)
        }
    }

    /// `&` anywhere in the constraint makes an output early-clobber.
    #[test]
    fn test_asm_constraint_early_clobber() {
        for c in ["=&r", "&=r", "+&r", "=&a"] {
            assert!(constraint(c, None).is_early_clobber(), "{c}");
        }
        for c in ["=r", "+r", "r", "=m", "0"] {
            assert!(!constraint(c, None).is_early_clobber(), "{c}");
        }
    }

    /// The input a `"+"` output implies is hidden; an explicit `"0"` is not.
    #[test]
    fn test_asm_constraint_hidden_readwrite_input() {
        assert!(constraint("+r", Some(0)).is_hidden_readwrite_input());
        assert!(constraint("+m", Some(1)).is_hidden_readwrite_input());
        assert!(!constraint("0", Some(0)).is_hidden_readwrite_input());
        assert!(!constraint("r", None).is_hidden_readwrite_input());
    }

    /// A register class, or a matching digit, wants a register; memory-only
    /// and immediate-only constraints do not.
    #[test]
    fn test_asm_constraint_wants_register() {
        for c in ["r", "=r", "+r", "=&r", "a", "0", "rm", "g", "ri"] {
            assert!(constraint(c, None).wants_register(), "{c}");
        }
        for c in ["m", "=m", "i", "n", "I"] {
            assert!(!constraint(c, None).wants_register(), "{c}");
        }
        // aarch64's register letter is a register class there.
        assert!(AsmConstraint::new(PseudoId(0), "=w", Arch::Aarch64, 64).wants_register());
    }

    /// Liveness asks `is_memory` of each output, so it must read the letters
    /// as the target does: a register alternative beside `m` makes the
    /// operand a value, and `Q` is memory on aarch64 but a register on
    /// x86-64.
    #[test]
    fn test_asm_constraint_is_memory_per_target() {
        let on = |c: &str, arch| AsmConstraint::new(PseudoId(0), c, arch, 64).is_memory();
        assert!(on("=m", Arch::X86_64) && on("=m", Arch::Aarch64));
        assert!(!on("+wm", Arch::Aarch64));
        assert!(on("Q", Arch::Aarch64));
        assert!(!on("=Q", Arch::X86_64));
        assert!(!on("xm", Arch::X86_64));

        // A register output defines its pseudo; it is no use of it.
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: vec![
                AsmConstraint::new(PseudoId(1), "+wm", Arch::Aarch64, 64),
                AsmConstraint::new(PseudoId(2), "=Q", Arch::Aarch64, 64),
            ],
            inputs: Vec::new(),
            clobbers: Vec::new(),
            goto_labels: Vec::new(),
        }));
        assert_eq!(asm.uses(), vec![PseudoId(2)]);
    }

    #[test]
    fn test_is_memory_barrier_asm_memory_clobber() {
        // `asm("..." ::: "memory")` — the heavily-used compiler
        // memory-barrier idiom. Must be a barrier.
        let with_mem = make_asm_with_clobbers(vec!["memory"]);
        assert!(with_mem.is_memory_barrier());

        // Mixed with other clobbers — still a barrier as long as
        // "memory" is in the list.
        let mixed = make_asm_with_clobbers(vec!["rax", "memory", "cc"]);
        assert!(mixed.is_memory_barrier());
    }

    #[test]
    fn test_is_memory_barrier_asm_without_memory_clobber() {
        // `asm("..." ::: "cc")` — clobbers condition codes only, not
        // a memory barrier. Surrounding loads/stores may legally be
        // reordered across it.
        let cc_only = make_asm_with_clobbers(vec!["cc"]);
        assert!(!cc_only.is_memory_barrier());

        // `asm("..." ::: "rax")` — register clobber only.
        let reg_only = make_asm_with_clobbers(vec!["rax"]);
        assert!(!reg_only.is_memory_barrier());

        // Asm with no clobber list at all.
        let bare = make_asm_with_clobbers(vec![]);
        assert!(!bare.is_memory_barrier());

        // Asm with no asm_data attached (degenerate; shouldn't
        // happen in well-formed IR but the predicate should be
        // robust).
        let no_data = Instruction::new(Opcode::Asm);
        assert!(!no_data.is_memory_barrier());
    }

    /// An instruction of `op` with every `PseudoId` and block slot filled,
    /// each with its own id, whether or not `op` uses the field.
    fn every_slot_filled(op: Opcode) -> Instruction {
        let operand =
            |pseudo: u32| AsmConstraint::new(PseudoId(pseudo), "r", Target::host().arch, 64);
        let mut insn = Instruction::new(op);
        insn.target = Some(PseudoId(1));
        insn.src = vec![PseudoId(2), PseudoId(3)];
        insn.phi_list = vec![
            (BasicBlockId(1), PseudoId(4)),
            (BasicBlockId(2), PseudoId(5)),
        ];
        insn.bb_true = Some(BasicBlockId(3));
        insn.bb_false = Some(BasicBlockId(4));
        let extra = insn.extra_mut();
        extra.indirect_target = Some(PseudoId(6));
        extra.lifetime_of = Some(PseudoId(7));
        extra.switch_cases = vec![(0, 0, BasicBlockId(5)), (1, 9, BasicBlockId(6))];
        extra.switch_default = Some(BasicBlockId(7));
        extra.asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: vec![operand(8), operand(9)],
            inputs: vec![operand(10), operand(11)],
            clobbers: Vec::new(),
            goto_labels: vec![(BasicBlockId(8), "l".to_string())],
        }));
        insn
    }

    /// `for_each_pseudo_mut` rewrites every slot `mentioned` reads, in its
    /// order, and then `lifetime_of` -- and nothing else: what it leaves
    /// alone reads back unchanged.
    #[test]
    fn test_for_each_pseudo_mut_visits_what_mentioned_reads() {
        for &op in Opcode::ALL {
            let mut insn = every_slot_filled(op);
            let mentioned: Vec<PseudoId> = insn.mentioned().collect();
            assert_eq!(
                mentioned,
                [1, 2, 3, 6, 4, 5, 8, 9, 10, 11].map(PseudoId),
                "{op:?}: target, sources, indirect target, phi operands, asm outputs then inputs"
            );

            let mut visited = Vec::new();
            insn.for_each_pseudo_mut(|p| {
                visited.push(*p);
                p.0 += 100;
            });
            let mut expected = mentioned.clone();
            expected.push(PseudoId(7));
            assert_eq!(visited, expected, "{op:?}: mentioned, then lifetime_of");

            let shifted: Vec<PseudoId> = mentioned.iter().map(|p| PseudoId(p.0 + 100)).collect();
            assert_eq!(insn.mentioned().collect::<Vec<_>>(), shifted, "{op:?}");
            assert_eq!(insn.extra().lifetime_of, Some(PseudoId(107)), "{op:?}");
        }

        // No extra box: nothing to visit there, and none is allocated.
        let mut bare = Instruction::new(Opcode::Add);
        bare.target = Some(PseudoId(1));
        bare.src = vec![PseudoId(2)];
        let mut visited = Vec::new();
        bare.for_each_pseudo_mut(|p| visited.push(*p));
        assert_eq!(visited, [PseudoId(1), PseudoId(2)]);
        assert!(bare.extra.is_none());
    }

    /// `for_each_block_mut` rewrites every block slot -- the control targets
    /// `control_targets` reads, then each phi operand's predecessor -- and
    /// leaves the operation width, type and position alone.
    #[test]
    fn test_for_each_block_mut_visits_every_block_slot() {
        for &op in Opcode::ALL {
            let mut insn = every_slot_filled(op);
            insn.size = 64;
            insn.pos = Some(Position {
                line: 7,
                ..Default::default()
            });
            assert_eq!(
                insn.control_targets(),
                [3, 4, 5, 6, 7, 8].map(BasicBlockId),
                "{op:?}"
            );

            let mut visited = Vec::new();
            insn.for_each_block_mut(|b| {
                visited.push(*b);
                b.0 += 100;
            });
            assert_eq!(
                visited,
                [3, 4, 5, 6, 7, 8, 1, 2].map(BasicBlockId),
                "{op:?}"
            );
            assert_eq!(
                insn.control_targets(),
                [103, 104, 105, 106, 107, 108].map(BasicBlockId),
                "{op:?}"
            );
            let preds: Vec<BasicBlockId> = insn.phi_list.iter().map(|&(b, _)| b).collect();
            assert_eq!(preds, [101, 102].map(BasicBlockId), "{op:?}");
            assert_eq!((insn.size, insn.pos.map(|p| p.line)), (64, Some(7)));
        }
    }
}
