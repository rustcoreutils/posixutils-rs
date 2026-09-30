//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The block moves whose source and destination cannot overlap
//
// `memmove` promises a right answer when its two blocks overlap, and `memcpy`
// does not (C17 7.24.2.1p2); where they cannot overlap the two are the same
// function, and `memcpy` is the one `memexpand` expands further and the
// library runs faster. gcc makes the same substitution. `memmove` and `bcopy`
// both reach the IR as a `Memmove`, so both are covered here.
//
// Two blocks cannot overlap when they are in different objects. The
// destination is known to be in another object than the source when:
// - the source is in an object nothing may write -- a string literal (C17
//   6.4.5p7) or a `const` object defined here (6.7.3p6), by the rule
//   `strdata` reads such objects by -- since a destination inside it would
//   be a write to it;
// - both are automatic objects of this function, and not the same one;
// - one is an automatic object and the other has a linker symbol.
// Two different named objects are not enough: an alias or a weak definition
// can put two names at one address.
//
// The rewrite happens in place: the operands and the result are the
// `Memmove`'s, and only the operation and the function it calls change.
//

use super::{callee_symbol, is_the_function, FoldCtx};
use crate::ir::memloc::{AddrMap, MemBase};
use crate::ir::strdata::ConstBytes;
use crate::ir::{Function, Instruction, Opcode};

/// The C name of the function a `Memcpy` calls.
const MEMCPY: &str = "memcpy";

/// Make every `Memmove` in `func` that cannot overlap a `Memcpy`. Answers
/// whether anything changed.
pub(super) fn run(func: &mut Function, ctx: &FoldCtx) -> bool {
    // `memcpy` written with `memmove` must not become a call to itself.
    if is_the_function(func, ctx, MEMCPY) {
        return false;
    }
    let am = AddrMap::build(func);
    let mut disjoint = Vec::new();
    for (b, bb) in func.blocks.iter().enumerate() {
        for (i, insn) in bb.insns.iter().enumerate() {
            if insn.op == Opcode::Memmove && cannot_overlap(func, &am, ctx.bytes, insn) {
                disjoint.push((b, i));
            }
        }
    }
    let symbol = callee_symbol(ctx, MEMCPY);
    for &(b, i) in &disjoint {
        let insn = &mut func.blocks[b].insns[i];
        insn.op = Opcode::Memcpy;
        insn.extra_mut().func_name = Some(symbol.to_string());
    }
    !disjoint.is_empty()
}

/// Whether the destination and source of the move `insn` are in different
/// objects.
fn cannot_overlap(func: &Function, am: &AddrMap, bytes: &ConstBytes, insn: &Instruction) -> bool {
    let &[dest, src, _] = insn.src.as_slice() else {
        return false;
    };
    let object = |p| am.resolve(func, p, 0, 0, None).base;
    match (object(dest), object(src)) {
        (_, MemBase::Global(name)) if bytes.is_read_only(&name) => true,
        (MemBase::Local(d), MemBase::Local(s)) => d != s,
        (MemBase::Local(_), MemBase::Global(_)) | (MemBase::Global(_), MemBase::Local(_)) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::{def, run_on};
    use super::*;
    use crate::ir::strdata::fixture::Fixture;
    use crate::ir::{GlobalDef, Initializer, Pseudo, PseudoId};
    use crate::types::Type;

    /// `memmove(dest, src, n)` as the linearizer makes it, and its result.
    fn memmove(fx: &mut Fixture, dest: PseudoId, src: PseudoId) -> PseudoId {
        let n = fx.unknown();
        let t = fx.fresh();
        let ptr = fx.types.void_ptr_id;
        fx.push(
            Instruction::new(Opcode::Memmove)
                .with_func("memmove")
                .with_target(t)
                .with_src3(dest, src, n)
                .with_type_and_size(ptr, 64),
        );
        t
    }

    /// The address of a new automatic `char[16]` of the function.
    fn local(fx: &mut Fixture, name: &str) -> PseudoId {
        let arr = fx.types.intern(Type::array(fx.types.char_id, 16));
        let f = fx.func();
        let sym = f.alloc_pseudo();
        f.add_pseudo(Pseudo::sym(sym, name.to_string()));
        f.add_local(name, sym, arr, None, None);
        let p = f.alloc_pseudo();
        let ptr = fx.types.char_ptr_id;
        fx.push(Instruction::sym_addr(p, sym, ptr));
        p
    }

    /// A global `long name[4]`, `const` or not, with an initializer, adjusted
    /// by `tweak`.
    fn global(fx: &mut Fixture, name: &str, is_const: bool, tweak: impl FnOnce(&mut GlobalDef)) {
        let arr = fx.types.intern(Type::array(fx.types.long_id, 4));
        let mut g = GlobalDef::new(name, arr, Initializer::Int(0));
        g.is_const = is_const;
        tweak(&mut g);
        fx.module.globals.push(g);
    }

    /// Fold `fx`'s function with `callees`, and answer what the move that
    /// defines `t` became: its operation and the function it calls.
    fn moved(fx: &mut Fixture, t: PseudoId, callees: &[(&'static str, &str)]) -> (Opcode, String) {
        let insns = run_on(fx, callees);
        let insn = def(&insns, t);
        (insn.op, insn.extra().func_name.clone().unwrap_or_default())
    }

    fn is_memcpy(fx: &mut Fixture, t: PseudoId) -> bool {
        let (op, callee) = moved(fx, t, &[]);
        let copy = op == Opcode::Memcpy;
        assert_eq!(callee, if copy { "memcpy" } else { "memmove" });
        copy
    }

    /// A move out of a string literal or a `const` object defined here --
    /// of any type, not only `char` -- is a copy, wherever it writes.
    #[test]
    fn a_move_out_of_a_read_only_object_is_a_copy() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abcde");
        let src = fx.addr(&lc);
        let dest = fx.unknown();
        let t = memmove(&mut fx, dest, src);
        assert!(is_memcpy(&mut fx, t));

        let mut fx = Fixture::new();
        global(&mut fx, "table", true, |_| {});
        let src = fx.addr("table");
        let dest = fx.unknown();
        let t = memmove(&mut fx, dest, src);
        assert!(is_memcpy(&mut fx, t));
    }

    /// A global that may change -- not `const`, or `const` but weak or
    /// tentative -- is no proof: the destination may be in it.
    #[test]
    fn a_move_out_of_an_object_that_may_change_stays_a_move() {
        type Tweak = fn(&mut GlobalDef);
        let tweaks: [(bool, Tweak); 3] = [
            (false, |_| {}),
            (true, |g| g.symbol_attrs.weak = true),
            (true, |g| g.init = Initializer::None),
        ];
        for (is_const, tweak) in tweaks {
            let mut fx = Fixture::new();
            global(&mut fx, "g", is_const, tweak);
            let src = fx.addr("g");
            let dest = fx.unknown();
            let t = memmove(&mut fx, dest, src);
            assert!(!is_memcpy(&mut fx, t), "const {is_const}");
        }
    }

    /// Two automatic objects are two blocks; one is not.
    #[test]
    fn two_locals_are_disjoint_and_one_is_not() {
        let mut fx = Fixture::new();
        let a = local(&mut fx, "a.0");
        let b = local(&mut fx, "b.1");
        let t = memmove(&mut fx, a, b);
        assert!(is_memcpy(&mut fx, t));

        let mut fx = Fixture::new();
        let a = local(&mut fx, "a.0");
        let four = fx.konst(4);
        let a4 = fx.op(Opcode::Add, a, four);
        let t = memmove(&mut fx, a4, a);
        assert!(!is_memcpy(&mut fx, t));
    }

    /// An automatic object and a named one are disjoint either way round;
    /// two named ones may be one object under two names.
    #[test]
    fn a_local_and_a_global_are_disjoint_but_two_globals_are_not() {
        for local_is_dest in [true, false] {
            let mut fx = Fixture::new();
            global(&mut fx, "g", false, |_| {});
            let g = fx.addr("g");
            let a = local(&mut fx, "a.0");
            let (dest, src) = if local_is_dest { (a, g) } else { (g, a) };
            let t = memmove(&mut fx, dest, src);
            assert!(is_memcpy(&mut fx, t), "local is dest: {local_is_dest}");
        }

        let mut fx = Fixture::new();
        global(&mut fx, "g", false, |_| {});
        global(&mut fx, "h", false, |_| {});
        let (g, h) = (fx.addr("g"), fx.addr("h"));
        let t = memmove(&mut fx, g, h);
        assert!(!is_memcpy(&mut fx, t));
    }

    /// Two pointers nothing is known about may overlap.
    #[test]
    fn unknown_pointers_stay_a_move() {
        let mut fx = Fixture::new();
        let (d, s) = (fx.unknown(), fx.unknown());
        let t = memmove(&mut fx, d, s);
        assert!(!is_memcpy(&mut fx, t));
    }

    /// The copy calls the program's name for `memcpy`, and never `memcpy`
    /// itself from inside it.
    #[test]
    fn the_copy_calls_the_programs_memcpy_but_never_itself() {
        let mut fx = Fixture::new();
        let lc = fx.literal("abc");
        let src = fx.addr(&lc);
        let dest = fx.unknown();
        let t = memmove(&mut fx, dest, src);
        let (op, callee) = moved(&mut fx, t, &[("memcpy", "my_memcpy")]);
        assert_eq!((op, callee.as_str()), (Opcode::Memcpy, "my_memcpy"));

        for (name, callees) in [
            ("memcpy", &[][..]),
            ("my_memcpy", &[("memcpy", "my_memcpy")][..]),
        ] {
            let mut fx = Fixture::new();
            fx.func().name = name.to_string();
            let lc = fx.literal("abc");
            let src = fx.addr(&lc);
            let dest = fx.unknown();
            let t = memmove(&mut fx, dest, src);
            assert_eq!(moved(&mut fx, t, callees).0, Opcode::Memmove, "in {name}");
        }
    }
}
