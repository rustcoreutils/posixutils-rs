//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// memcpy, memset and memmove of a small constant length, as loads and stores
//
// C17 7.1.4p1 lets an implementation compute a library function in place, and
// for these three gcc does at every level it optimizes: `memcpy(d, s, 16)` is
// one 16-byte load and store, `memset(p, 0, 32)` two stores. Here the
// expansion is ordinary IR -- integer loads and stores of 8, 4, 2 and 1
// bytes -- rather than something only a backend sees, so every pass after it
// treats the bytes as what they are: `loadfwd` forwards a stored value into
// the load that reads it back, `dse` removes a store nothing reads, and a
// local whose address went only to `memcpy` no longer escapes.
//
// It runs after inlining and again inside the optimizer's loop, so a length
// that only becomes a constant there -- `cp(d, s, 24)` of a `static` wrapper
// around `memcpy(d, s, n)` -- is expanded too. A length above the limit, or
// one not known at compile time, stays a call.
//
// No alignment is assumed of either pointer: an unaligned integer load or
// store is allowed on both targets. Nor is volatility honoured, because
// `memcpy` never promised it -- the library accesses the bytes however it
// likes, and a `volatile` object reaches it only through a conversion to
// `void *` that has already dropped the qualifier.
//

use super::build::Builder;
use super::facts::ConstMap;
use super::{Function, Instruction, Opcode, PseudoId};
use crate::types::{TypeId, TypeTable};

/// The most bytes a `memcpy` or `memset` of a constant length is expanded
/// into, and the most an aggregate copy the linearizer emits is.
///
/// gcc -O2 goes further (256 bytes of `memcpy` is sixteen 16-byte SSE or
/// `q`-register pairs on either target), but c17 moves at most 8 bytes at a
/// time, so 128 bytes is already sixteen load/store pairs -- the same count
/// of instructions as gcc's 256, and about what one call and its argument
/// set-up cost once the clobbered caller-saved registers are counted.
///
/// There has to be a limit. Each chunk is a load, a store and a pseudo: a
/// 256 KB struct passed by value -- `struct big { int i[0x10000]; }`, which
/// gcc.c-torture's pr28982b does -- once produced 65,536 IR instructions and
/// a 65-second compile against gcc's one `call memcpy`.
pub(crate) const INLINE_LIMIT_BYTES: i64 = 128;

/// The most bytes a `memmove` of a constant length is expanded into.
///
/// Every byte is loaded before any is stored, which is what makes an
/// overlapping move right in both directions, and it means every chunk is
/// live at once: 64 bytes is eight 8-byte values, which fits the general
/// registers either target has free without spilling. gcc stops at a single
/// move (16 bytes); c17 has no 16-byte move, and its eight are still cheaper
/// than the call.
const MOVE_LIMIT_BYTES: i64 = 64;

/// One of the widths a block is moved in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Chunk {
    B8,
    B4,
    B2,
    B1,
}

impl Chunk {
    pub(crate) fn bytes(self) -> i64 {
        match self {
            Chunk::B8 => 8,
            Chunk::B4 => 4,
            Chunk::B2 => 2,
            Chunk::B1 => 1,
        }
    }

    pub(crate) fn bits(self) -> u32 {
        self.bytes() as u32 * 8
    }

    /// The unsigned integer type the chunk is moved as.
    pub(crate) fn typ(self, types: &TypeTable) -> TypeId {
        match self {
            Chunk::B8 => types.ulong_id,
            Chunk::B4 => types.uint_id,
            Chunk::B2 => types.ushort_id,
            Chunk::B1 => types.uchar_id,
        }
    }
}

/// The pieces a block of `bytes` is moved in, as (offset, width), in rising
/// offset: as many 8-byte chunks as fit, then at most one each of 4, 2 and 1.
pub(crate) fn block_chunks(bytes: i64) -> impl Iterator<Item = (i64, Chunk)> {
    let mut offset = 0;
    std::iter::from_fn(move || {
        let chunk = [Chunk::B8, Chunk::B4, Chunk::B2, Chunk::B1]
            .into_iter()
            .find(|c| bytes - offset >= c.bytes())?;
        let at = offset;
        offset += chunk.bytes();
        Some((at, chunk))
    })
}

/// How to read a whole object of `bytes` bytes into one register when
/// `bytes` is not a natural access width -- 3, 5, 6 or 7, which is what a
/// small composite gives.
///
/// The answer is a pair of *overlapping* power-of-two accesses, returned as
/// `(width, high_offset)`: one of `width` bytes at offset 0, and one of
/// `width` bytes at `high_offset`, chosen so the second ends exactly on the
/// object's last byte. The two overlap in the middle, and the overlapping
/// bytes are read twice with the same value, so OR-ing the halves together
/// (after shifting the high one up by `high_offset * 8`) reproduces the
/// object exactly -- and neither access reaches past its end.
///
/// `None` for a size that is already one access: 1, 2, 4 and 8, and anything
/// larger than a register, which is copied rather than loaded.
///
/// Rounding a ragged size up to the next width instead is what the back ends
/// used to do, and it reads up to three bytes past the object -- which faults
/// when the object ends a page.
pub(crate) fn overlapping_halves(bits: u32) -> Option<(i64, i64)> {
    let bytes = i64::from(bits / 8);
    if bits % 8 != 0 || bytes == 0 || bytes > 8 || bytes.count_ones() == 1 {
        return None;
    }
    let width = 1i64 << (63 - bytes.leading_zeros() as i64);
    Some((width, bytes - width))
}

/// The three block memory operations, and how far each is expanded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockOp {
    Copy,
    Set,
    Move,
}

impl BlockOp {
    fn of(op: Opcode) -> Option<BlockOp> {
        match op {
            Opcode::Memcpy => Some(BlockOp::Copy),
            Opcode::Memset => Some(BlockOp::Set),
            Opcode::Memmove => Some(BlockOp::Move),
            _ => None,
        }
    }

    fn limit(self) -> i64 {
        match self {
            BlockOp::Copy | BlockOp::Set => INLINE_LIMIT_BYTES,
            BlockOp::Move => MOVE_LIMIT_BYTES,
        }
    }
}

/// Expand every block memory operation in `func` whose length is a small
/// constant. Answers whether anything changed.
pub fn run(func: &mut Function, types: &TypeTable) -> bool {
    let consts = ConstMap::new(func);
    let mut changed = false;
    for b in 0..func.blocks.len() {
        let insns = std::mem::take(&mut func.blocks[b].insns);
        let mut out = Vec::with_capacity(insns.len());
        for insn in insns {
            match expandable(&insn, &consts) {
                Some((op, n)) => {
                    Expander {
                        b: Builder::new(&mut *func, types, insn.pos, &mut out),
                        consts: &consts,
                        model: &insn,
                    }
                    .expand(op, n);
                    changed = true;
                }
                None => out.push(insn),
            }
        }
        func.blocks[b].insns = out;
    }
    changed
}

/// The operation `insn` is and its length, when it is one to expand.
fn expandable(insn: &Instruction, consts: &ConstMap) -> Option<(BlockOp, i64)> {
    let op = BlockOp::of(insn.op)?;
    let &[_, _, n] = insn.src.as_slice() else {
        return None;
    };
    let n = i64::try_from(consts.get_at(n, 64, false)?).ok()?;
    (0..=op.limit()).contains(&n).then_some((op, n))
}

/// The instructions that replace one block memory operation.
struct Expander<'a> {
    b: Builder<'a>,
    consts: &'a ConstMap,
    /// The instruction being replaced: its operands and its result.
    model: &'a Instruction,
}

impl Expander<'_> {
    fn expand(mut self, op: BlockOp, n: i64) {
        let (dest, second) = (self.model.src[0], self.model.src[1]);
        match op {
            BlockOp::Copy => {
                for (at, chunk) in block_chunks(n) {
                    let v = self.load(second, at, chunk);
                    self.store(v, dest, at, chunk);
                }
            }
            BlockOp::Move => {
                let loaded: Vec<_> = block_chunks(n)
                    .map(|(at, chunk)| (self.load(second, at, chunk), at, chunk))
                    .collect();
                for (v, at, chunk) in loaded {
                    self.store(v, dest, at, chunk);
                }
            }
            BlockOp::Set => {
                let mut fill = Fill::new(second, self.consts);
                for (at, chunk) in block_chunks(n) {
                    let v = fill.at(&mut self.b, chunk);
                    self.store(v, dest, at, chunk);
                }
            }
        }
        // Each returns its destination.
        if let Some(target) = self.model.target {
            let typ = self.model.typ.unwrap_or(self.b.types.void_ptr_id);
            self.b.copy_into(target, dest, typ, 64);
        }
    }

    fn load(&mut self, addr: PseudoId, at: i64, chunk: Chunk) -> PseudoId {
        let typ = chunk.typ(self.b.types);
        self.b.load(addr, at, typ, chunk.bits())
    }

    fn store(&mut self, v: PseudoId, addr: PseudoId, at: i64, chunk: Chunk) {
        let typ = chunk.typ(self.b.types);
        self.b.store(v, addr, at, typ, chunk.bits());
    }
}

/// The value `memset` stores: its `int` operand converted to `unsigned char`
/// (C17 7.24.6.1p2), repeated across each chunk's width.
struct Fill {
    /// The `int` operand.
    c: PseudoId,
    /// The byte, when the operand is a constant.
    byte: Option<u8>,
    /// The byte repeated across 64 bits, once computed.
    word: Option<PseudoId>,
    /// The value for each chunk width, once computed, indexed widest first.
    at_width: [Option<PseudoId>; 4],
}

/// Every byte of a 64-bit word 1: a byte times this is the byte repeated.
const BYTE_REPEAT: i128 = 0x0101_0101_0101_0101;

impl Fill {
    fn new(c: PseudoId, consts: &ConstMap) -> Fill {
        Fill {
            c,
            byte: consts.get_at(c, 8, false).map(|v| v as u8),
            word: None,
            at_width: [None; 4],
        }
    }

    fn at(&mut self, ex: &mut Builder<'_>, chunk: Chunk) -> PseudoId {
        let slot = chunk as usize;
        if let Some(v) = self.at_width[slot] {
            return v;
        }
        let typ = chunk.typ(ex.types);
        let v = match self.byte {
            Some(byte) => ex.constant(i128::from(byte) * BYTE_REPEAT, typ, chunk.bits()),
            None => {
                let word = self.word(ex);
                match chunk {
                    Chunk::B8 => word,
                    _ => ex.convert(
                        Opcode::Trunc,
                        word,
                        (ex.types.ulong_id, 64),
                        (typ, chunk.bits()),
                    ),
                }
            }
        };
        self.at_width[slot] = Some(v);
        v
    }

    /// `(unsigned long)(c & 0xff) * 0x0101010101010101`.
    fn word(&mut self, ex: &mut Builder<'_>) -> PseudoId {
        if let Some(w) = self.word {
            return w;
        }
        let (uint, ulong) = (ex.types.uint_id, ex.types.ulong_id);
        let mask = ex.constant(0xff, uint, 32);
        let byte = ex.binop(Opcode::And, self.c, mask, uint, 32);
        let wide = ex.convert(Opcode::Zext, byte, (uint, 32), (ulong, 64));
        let repeat = ex.constant(BYTE_REPEAT, ulong, 64);
        let w = ex.binop(Opcode::Mul, wide, repeat, ulong, 64);
        self.word = Some(w);
        w
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::constfold::at_width;
    use crate::ir::{BasicBlock, BasicBlockId, Pseudo, PseudoKind};
    use crate::target::Target;

    #[test]
    fn chunks_cover_the_block_widest_first() {
        let pieces = |n| {
            block_chunks(n)
                .map(|(at, c)| (at, c.bytes()))
                .collect::<Vec<_>>()
        };
        assert_eq!(pieces(0), vec![]);
        assert_eq!(pieces(1), vec![(0, 1)]);
        assert_eq!(pieces(7), vec![(0, 4), (4, 2), (6, 1)]);
        assert_eq!(pieces(16), vec![(0, 8), (8, 8)]);
        assert_eq!(pieces(23), vec![(0, 8), (8, 8), (16, 4), (20, 2), (22, 1)]);
        for n in 0..=200 {
            let mut next = 0;
            for (at, c) in block_chunks(n) {
                assert_eq!(at, next, "{n}: a gap or an overlap");
                next += c.bytes();
            }
            assert_eq!(next, n, "{n}: not covered");
        }
    }

    /// `f(dest, second, c)` with one block operation `op` of length `n`,
    /// its result returned. `n` is a constant reached through a `Copy`, the
    /// shape SCCP leaves; `n = None` makes it the third argument instead.
    fn block_op(op: Opcode, n: Option<i128>, c: Option<i128>) -> (Function, TypeTable) {
        let types = TypeTable::new(&Target::host());
        let vp = types.void_ptr_id;
        let mut f = Function::new("f", vp);
        for (i, name) in ["d", "s", "n"].iter().enumerate() {
            f.add_param(*name, vp);
            f.add_pseudo(Pseudo::arg(PseudoId(i as u32), i as u32));
        }
        f.next_pseudo = 10;
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        let len = match n {
            Some(n) => {
                let k = f.create_const_pseudo(n);
                let copy = f.alloc_pseudo();
                bb.add_insn(Instruction::unop(Opcode::Copy, copy, k, types.ulong_id, 64));
                copy
            }
            None => PseudoId(2),
        };
        let second = match c {
            Some(c) => f.create_const_pseudo(c),
            None => PseudoId(1),
        };
        let result = f.alloc_pseudo();
        bb.add_insn(
            Instruction::new(op)
                .with_func(op.name())
                .with_target(result)
                .with_src3(PseudoId(0), second, len)
                .with_type_and_size(vp, 64),
        );
        bb.add_insn(Instruction::ret_typed(Some(result), vp, 64));
        f.blocks.push(bb);
        f.entry = BasicBlockId(0);
        (f, types)
    }

    fn ops(f: &Function) -> Vec<Opcode> {
        f.blocks[0].insns.iter().map(|i| i.op).collect()
    }

    fn count(f: &Function, op: Opcode) -> usize {
        ops(f).iter().filter(|&&o| o == op).count()
    }

    /// The value of the constant `id`, if it is one.
    fn val(f: &Function, id: PseudoId) -> Option<i128> {
        f.get_pseudo(id).and_then(|p| match p.kind {
            PseudoKind::Val(v) => Some(v),
            _ => None,
        })
    }

    #[test]
    fn memcpy_becomes_loads_and_stores_and_returns_its_destination() {
        let (mut f, types) = block_op(Opcode::Memcpy, Some(23), None);
        assert!(run(&mut f, &types));
        assert_eq!(count(&f, Opcode::Memcpy), 0);
        let widths: Vec<(i64, u32)> = f.blocks[0]
            .insns
            .iter()
            .filter(|i| i.op == Opcode::Store)
            .map(|i| (i.offset, i.size))
            .collect();
        assert_eq!(widths, vec![(0, 64), (8, 64), (16, 32), (20, 16), (22, 8)]);
        // Each store is of the load just before it, from the source.
        let insns = &f.blocks[0].insns;
        for (i, s) in insns
            .iter()
            .enumerate()
            .filter(|(_, i)| i.op == Opcode::Store)
        {
            let l = &insns[i - 1];
            assert_eq!(l.op, Opcode::Load);
            assert_eq!(
                (l.src[0], l.offset, l.size),
                (PseudoId(1), s.offset, s.size)
            );
            assert_eq!((s.src[0], s.src[1]), (PseudoId(0), l.target.unwrap()));
        }
        // The result is the destination.
        let ret = insns.iter().find(|i| i.op == Opcode::Ret).unwrap();
        let def = insns.iter().find(|i| i.target == Some(ret.src[0])).unwrap();
        assert_eq!((def.op, def.src[0]), (Opcode::Copy, PseudoId(0)));
    }

    #[test]
    fn memmove_loads_everything_before_storing_anything() {
        let (mut f, types) = block_op(Opcode::Memmove, Some(20), None);
        assert!(run(&mut f, &types));
        let o = ops(&f);
        let last_load = o.iter().rposition(|&x| x == Opcode::Load).unwrap();
        let first_store = o.iter().position(|&x| x == Opcode::Store).unwrap();
        assert!(last_load < first_store, "{o:?}");
        assert_eq!(count(&f, Opcode::Load), 3);
        assert_eq!(count(&f, Opcode::Store), 3);
        assert_eq!(count(&f, Opcode::Memmove), 0);
    }

    #[test]
    fn memset_of_a_constant_byte_stores_it_repeated() {
        let (mut f, types) = block_op(Opcode::Memset, Some(15), Some(0x1ab));
        assert!(run(&mut f, &types));
        let stored: Vec<(u32, Option<i128>)> = f.blocks[0]
            .insns
            .iter()
            .filter(|i| i.op == Opcode::Store)
            .map(|i| (i.size, val(&f, i.src[1])))
            .collect();
        assert_eq!(
            stored,
            vec![
                (64, Some(at_width(0xabab_abab_abab_abab, 64, true))),
                (32, Some(at_width(0xabab_abab, 32, true))),
                (16, Some(at_width(0xabab, 16, true))),
                (8, Some(at_width(0xab, 8, true))),
            ]
        );
    }

    #[test]
    fn memset_of_a_variable_byte_multiplies_it_out_once() {
        let (mut f, types) = block_op(Opcode::Memset, Some(31), None);
        assert!(run(&mut f, &types));
        assert_eq!(count(&f, Opcode::And), 1);
        assert_eq!(count(&f, Opcode::Zext), 1);
        assert_eq!(count(&f, Opcode::Mul), 1);
        // 8, 8, 8, then one each of 4, 2 and 1, each narrowed once.
        assert_eq!(count(&f, Opcode::Store), 6);
        assert_eq!(count(&f, Opcode::Trunc), 3);
        let insns = &f.blocks[0].insns;
        let mul = insns.iter().find(|i| i.op == Opcode::Mul).unwrap();
        assert_eq!(val(&f, mul.src[1]), Some(0x0101_0101_0101_0101));
        let and = insns.iter().find(|i| i.op == Opcode::And).unwrap();
        assert_eq!((and.src[0], val(&f, and.src[1])), (PseudoId(1), Some(0xff)));
    }

    #[test]
    fn a_long_or_unknown_length_stays_a_call() {
        for (op, n) in [
            (Opcode::Memcpy, Some(129)),
            (Opcode::Memset, Some(129)),
            (Opcode::Memmove, Some(65)),
            (Opcode::Memcpy, None),
            (Opcode::Memset, None),
            (Opcode::Memmove, None),
        ] {
            let (mut f, types) = block_op(op, n, Some(0));
            assert!(!run(&mut f, &types), "{op:?} {n:?}");
            assert_eq!(count(&f, op), 1);
        }
        for (op, n) in [
            (Opcode::Memcpy, 128),
            (Opcode::Memset, 128),
            (Opcode::Memmove, 64),
        ] {
            let (mut f, types) = block_op(op, Some(n), Some(0));
            assert!(run(&mut f, &types), "{op:?} {n}");
            assert_eq!(count(&f, op), 0);
        }
    }

    #[test]
    fn a_zero_length_leaves_only_the_result() {
        let (mut f, types) = block_op(Opcode::Memcpy, Some(0), None);
        assert!(run(&mut f, &types));
        assert_eq!(count(&f, Opcode::Load) + count(&f, Opcode::Store), 0);
        assert_eq!(count(&f, Opcode::Memcpy), 0);
        assert!(ops(&f).contains(&Opcode::Copy));
    }

    /// The ragged widths are the ones a small composite gives; each pair must
    /// cover the object exactly and end on its last byte, never past it.
    #[test]
    fn overlapping_halves_covers_a_ragged_size_without_passing_its_end() {
        for (bytes, want) in [(3i64, (2, 1)), (5, (4, 1)), (6, (4, 2)), (7, (4, 3))] {
            let got = overlapping_halves(bytes as u32 * 8);
            assert_eq!(
                got,
                Some(want),
                "{bytes} bytes: split as {got:?}, expected {want:?}"
            );
            let (width, high) = got.unwrap();
            assert_eq!(
                width + high,
                bytes,
                "{bytes} bytes: the high half at {high} spans {width} and so ends \
                 at {}, not on the last byte",
                width + high
            );
            assert!(
                high < width,
                "{bytes} bytes: the halves at 0 and {high} leave a {} byte gap \
                 rather than overlapping",
                high - width
            );
        }
    }

    /// A size that is already one access must stay one access, and anything
    /// wider than a register is copied, not loaded.
    #[test]
    fn overlapping_halves_declines_a_size_that_is_already_one_access() {
        for bits in [0, 8, 16, 32, 64, 72, 96, 128, 40 + 4] {
            assert_eq!(
                overlapping_halves(bits),
                None,
                "{bits} bits was split, but it is not a ragged sub-register size"
            );
        }
    }
}
