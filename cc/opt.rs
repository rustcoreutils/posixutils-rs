//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Optimization pass runner and the utilities its passes share.
//

use gettextrs::gettext;
use std::collections::{BTreeMap, HashSet};

use crate::diag::Position;
use crate::ir::asm_operand;
use crate::ir::constglobal;
use crate::ir::copyprop;
use crate::ir::dce;
use crate::ir::dse;
use crate::ir::ifconv;
use crate::ir::inline;
use crate::ir::instcombine;
use crate::ir::libcall_fold;
use crate::ir::loadfwd;
use crate::ir::mem2reg::mem2reg;
use crate::ir::memexpand;
use crate::ir::memloc;
use crate::ir::objsize::{self, ObjectSizes, Settle};
use crate::ir::sccp;
use crate::ir::vrp;
use crate::ir::{Function, Module, Opcode};
use crate::target::Target;
use crate::types::TypeTable;

// What optimization was asked for

/// The optimization the command line asked for.
///
/// Built once from `-O...` and handed to *both* the optimizer and the
/// preprocessor, so that `__OPTIMIZE__` and its siblings are derived from the
/// value that drives behaviour rather than from a parallel copy of it: a
/// capability macro must never be able to disagree with the capability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Optimization {
    /// 0-3, as in `-O0`..`-O3`. `-Og` is 1 and `-Os` is 2.
    level: u8,
    /// `-Os`: prefer smaller code where the choice arises. Read by
    /// `__OPTIMIZE_SIZE__`; c17 makes no size-vs-speed choice of its own yet.
    for_size: bool,
    /// `-fno-inline`: general inlining is off whatever the level says.
    ///
    /// Separate from the level because GCC keeps them separate: `-O2
    /// -fno-inline` still optimizes, and still defines `__OPTIMIZE__`, while
    /// also defining `__NO_INLINE__`.
    no_inline: bool,
}

impl Optimization {
    /// Parse the argument of `-O`, as clap's `value_parser`.
    ///
    /// Every spelling GCC and Clang accept is recognised, so that `-Ofast`
    /// reaches this function rather than being mistaken for a source file
    /// operand. See [`is_valid_opt_level`].
    pub fn from_flag(spelling: &str) -> Result<Self, String> {
        let (level, for_size) = match spelling {
            "0" => (0, false),
            "1" => (1, false),
            "2" => (2, false),
            "3" => (3, false),
            // GCC's "optimize for debugging": -O1 minus the passes that
            // confuse a debugger. c17 has no such pass to drop, so the level
            // is all that carries over.
            "g" => (1, false),
            // -Os is -O2 without the size-increasing choices. c17 makes no
            // size-vs-speed choice yet, so the flag records the intent and
            // sets `__OPTIMIZE_SIZE__` for the headers that read it.
            "s" => (2, true),
            // `-Ofast` is `-O3` plus permission to relax IEEE arithmetic, and
            // `-Oz` is `-Os` pushed further. c17 has neither extra, so it
            // takes the level it does have and says what it did not do.
            //
            // Refusing them outright was the older answer and was the wrong
            // one twice over. It failed builds for a flag whose only effect
            // here would have been *more* speed, never a different answer --
            // stricter floating-point semantics cannot make a correct program
            // wrong. And it disagreed with c17's own treatment of
            // `-ffast-math`, which is accepted and ignored with a note, so
            // `-O3 -ffast-math` built and the single flag that means the same
            // thing did not.
            "fast" => {
                eprintln!(
                    "c17: {}",
                    gettext(
                        "-Ofast: compiling at -O3; IEEE arithmetic is not relaxed, \
                         c17 has no fast-math mode"
                    )
                );
                (3, false)
            }
            "z" => {
                eprintln!(
                    "c17: {}",
                    gettext("-Oz: compiling at -Os; c17 has no smaller tier")
                );
                (2, true)
            }
            other => return Err(format!("invalid optimization level '{other}'")),
        };
        Ok(Self {
            level,
            for_size,
            no_inline: false,
        })
    }

    /// Whether any optimization runs at all. Drives `__OPTIMIZE__`.
    pub fn optimizes(&self) -> bool {
        self.level > 0
    }

    /// Whether a single-call-site function may be inlined up to the large
    /// ceiling rather than the small one -- what `-O2` buys over `-O1`.
    pub fn inlines_aggressively(&self) -> bool {
        self.level >= 2
    }

    /// Whether code size is preferred over speed. Drives `__OPTIMIZE_SIZE__`.
    pub fn for_size(&self) -> bool {
        self.for_size
    }

    /// Whether functions are inlined on their merits.
    ///
    /// False at `-O0` and under `-fno-inline`. `__attribute__((always_inline))`
    /// is *not* covered by this and still fires either way -- that is GCC's
    /// behaviour, and glibc's `__fortify_function` depends on it. Drives
    /// `__NO_INLINE__`, which GCC likewise defines while still honouring the
    /// attribute.
    pub fn inlines_generally(&self) -> bool {
        self.optimizes() && !self.no_inline
    }

    /// Record `-fno-inline` / `-finline`, last one winning.
    pub fn set_inlining(&mut self, enabled: bool) {
        self.no_inline = !enabled;
    }
}

/// Maximum iterations for the optimization fixed-point loop.
/// Prevents infinite loops if passes keep making changes.
const MAX_ITERATIONS: usize = 10;

/// What the per-function passes share.
struct PassCtx<'a> {
    types: &'a TypeTable,
    known: &'a constglobal::KnownGlobals,
    mi: &'a memloc::ModuleInfo,
    fold: &'a libcall_fold::FoldCtx<'a>,
    /// The sizes of the module's named objects, for `objsize`.
    sizes: &'a ObjectSizes,
}

/// One per-function pass: its name, and a run that answers whether it
/// changed the function.
type Pass = (&'static str, fn(&mut Function, &PassCtx) -> bool);

/// The fixed-point loop's passes, in order. The order is load-bearing: each
/// pass hands the next one a shape it could not have seen for itself.
const PASSES: [Pass; 13] = [
    // `constglobal` before anything looks at a value: a load of a `const`
    // global becomes its initializer, which every pass below treats as the
    // constant it is.
    ("constglobal", |f, c| constglobal::run(f, c.types, c.known)),
    // `memexpand` ahead of all of them, so the loads and stores it makes of a
    // length SCCP has only now proved constant are forwarded and killed in
    // the same iteration.
    ("memexpand", |f, c| memexpand::run(f, c.types)),
    // `loadfwd`: turning a load into a copy of a stored value is what gives
    // every pass below it something to fold, and a value that came out of
    // memory is otherwise opaque to all of them. It does *gain* from a second
    // iteration -- an index expression reaches it as
    // `add %sym, (mul (sext 1) 4)` and only becomes a constant displacement
    // once `instcombine` has folded the multiply -- which is why it sits
    // inside the loop rather than ahead of it.
    ("loadfwd", |f, c| loadfwd::run(f, c.types, c.mi)),
    // `vrp` before `ifconv`, because it is the only pass that reads a
    // *branch*: `var <= 0` being false says `var >= 1` on that edge, and
    // `ifconv` collapses exactly that diamond into a `Select`, speculating
    // the arm into a predecessor where `var` is unconstrained. Once that has
    // happened the comparison is genuinely undecidable -- `var == 0` makes
    // `(unsigned)(var - 1)` equal `UINT_MAX` -- so nothing downstream
    // recovers it.
    ("vrp", |f, _| vrp::run(f)),
    // `ifconv` collapses a short-circuit diamond into a `Select` in one
    // block, which is what makes the two relationals inside it comparable
    // at all.
    ("ifconv", |f, c| ifconv::run(f, c.types)),
    // `objsize` answers each `__builtin_object_size` whose object the
    // passes above have uncovered -- a load forwarded, a choice collapsed --
    // so that `sccp` sees its answer as the constant it is, and
    // `libcall_fold` sees the size a `_chk` call checks against. One whose
    // object is still unknown waits for the end: see `optimize_function`.
    ("objsize", |f, c| {
        objsize::run(f, c.types, c.sizes, Settle::Known)
    }),
    // `sccp` proves branches dead, which `instcombine` cannot, and leaves
    // behind `Copy` from a constant -- exactly the shape `instcombine`'s
    // `ConstMap` follows.
    ("sccp", |f, _| sccp::run(f)),
    // `instcombine` inside the loop rather than once before it, because it
    // derives constants SCCP structurally cannot (`x - x`, `x ^ x`), any of
    // which can make a branch condition constant and send SCCP round again.
    ("instcombine", |f, c| instcombine::run(f, c.types)),
    // `libcall_fold` once the arguments are as constant as `sccp` and
    // `instcombine` can make them; what it leaves -- a constant, a load of
    // one byte, a `Select` -- is theirs and `loadfwd`'s next round.
    ("libcall_fold", |f, c| libcall_fold::run(f, c.fold)),
    // `copyprop` once everything above has made its copies, so that `dce`
    // below collects the ones it leaves unused.
    ("copyprop", |f, c| copyprop::run(f, c.types)),
    // `dse` before `dce`, so the value chain feeding a killed store is swept
    // in the same iteration rather than surviving to the next one.
    ("dse", |f, c| dse::run(f, c.types, c.mi)),
    // `dce` last: SCCP removes a dead edge but deletes no block, and leaves
    // the `PhiSource` of a folded phi for `dce` to collect.
    ("dce", |f, _| dce::run(f)),
    // `simplify_cfg` once `dce` has dropped what made a block more than a
    // branch: the next round's passes see fewer, longer blocks.
    ("simplify_cfg", |f, _| f.simplify_cfg()),
];

/// One round of the passes that need nothing module-wide, run on every
/// function before inlining: the inliner sizes a callee by the code it
/// emits, so it should see the code that will be emitted -- not branches
/// SCCP is about to delete or copies `copyprop` is about to forward.
const BEFORE_INLINING: [fn(&mut Function, &TypeTable) -> bool; 5] = [
    |f, _| sccp::run_before_inlining(f),
    instcombine::run,
    copyprop::run,
    |f, _| dce::run(f),
    |f, _| f.simplify_cfg(),
];

fn simplify_before_inlining(func: &mut Function, types: &TypeTable) {
    for run in BEFORE_INLINING {
        run(func, types);
    }
    func.remove_nops();
}

/// How a function's fixed-point loop went.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Convergence {
    pub function: String,
    pub iterations: usize,
    /// How many iterations each pass changed the function in, by its
    /// position in `PASSES`.
    pub changes: [u32; PASSES.len()],
    /// The passes that changed the function in the last iteration: empty
    /// when the loop reached its fixed point, and otherwise what was still
    /// moving when it was cut off.
    pub still_changing: Vec<&'static str>,
}

impl Convergence {
    pub fn converged(&self) -> bool {
        self.still_changing.is_empty()
    }
}

/// What the optimizer did, for the functions worth reporting.
#[derive(Debug, Default)]
pub struct OptReport {
    /// Functions whose loop ran out of iterations before its fixed point.
    /// What is left is correct, only less optimized than it could be.
    pub unconverged: Vec<Convergence>,
}

// Pass Runner

/// Where a forwarding function names its caller's variadic arguments: the
/// first `__builtin_va_arg_pack()` or `__builtin_va_arg_pack_len()` in its
/// body, by spelling and position.
#[derive(Debug, Clone, Copy, PartialEq)]
struct PackUse {
    builtin: &'static str,
    pos: Position,
}

/// The first pack builtin `func` uses, or `None` for a function that forwards
/// nothing.
fn pack_use(func: &Function) -> Option<PackUse> {
    func.blocks.iter().flat_map(|b| &b.insns).find_map(|i| {
        Some(PackUse {
            builtin: i.va_arg_pack_builtin()?,
            pos: i.pos.unwrap_or_default(),
        })
    })
}

/// Whether `func` names its caller's variadic arguments -- that is, whether
/// its body contains `__builtin_va_arg_pack()` or `__builtin_va_arg_pack_len()`.
fn forwards_caller_arguments(func: &Function) -> bool {
    pack_use(func).is_some()
}

/// What would need an out-of-line copy of a forwarding function.
#[derive(Debug, Clone, Copy, PartialEq)]
enum CopyDemand {
    /// The function is an external definition, which must be emitted.
    ExternalDefinition,
    /// Its address survives optimization, so something may call it there.
    AddressTaken,
}

/// A forwarding function something needs as an object of its own, which it
/// cannot be: what it forwards exists only at a call site it is inlined into.
#[derive(Debug, PartialEq)]
struct ForwarderCopy {
    function: String,
    pack: PackUse,
    /// Where the copy is asked for: the address-taking instruction, or the
    /// pack builtin when nothing nearer has a position.
    pos: Position,
    why: CopyDemand,
}

/// The forwarding functions that are external definitions.
///
/// Asked before `suppress_forwarding_bodies` clears `emit`, which until then
/// says whether the function provides its definition: an inline definition
/// (C17 6.7.4p7, or `gnu_inline`) leaves that to another translation unit,
/// and a `static` one needs a copy only if its address is taken.
fn external_forwarders(module: &Module) -> Vec<ForwarderCopy> {
    module
        .functions
        .iter()
        .filter(|f| f.emit && !f.is_static)
        .filter_map(|f| {
            pack_use(f).map(|pack| ForwarderCopy {
                function: f.name.clone(),
                pack,
                pos: pack.pos,
                why: CopyDemand::ExternalDefinition,
            })
        })
        .collect()
}

/// The `static` forwarding functions, whose address would need a copy.
///
/// Recorded before optimization, which may rewrite a forwarder's own body.
fn static_forwarders(module: &Module) -> BTreeMap<String, PackUse> {
    module
        .functions
        .iter()
        .filter(|f| f.emit && f.is_static)
        .filter_map(|f| pack_use(f).map(|pack| (f.name.clone(), pack)))
        .collect()
}

/// The forwarders in `forwarders` whose address is still taken: by an emitted
/// function, or in a global's initializer. One each, at the first place found.
///
/// Asked after optimization, as gcc does: an address that is never used is
/// deleted at `-O1` and above, and then nothing needs the copy.
fn forwarder_addresses(
    module: &Module,
    forwarders: &BTreeMap<String, PackUse>,
) -> Vec<ForwarderCopy> {
    let mut found: BTreeMap<&str, Position> = BTreeMap::new();
    for func in module.functions.iter().filter(|f| f.emit) {
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            if insn.op != Opcode::SymAddr {
                continue;
            }
            let Some(name) = insn.src.first().and_then(|&s| func.global_sym_name(s)) else {
                continue;
            };
            if let Some((name, pack)) = forwarders.get_key_value(name) {
                found
                    .entry(name.as_str())
                    .or_insert(insn.pos.unwrap_or(pack.pos));
            }
        }
    }
    // A global's initializer has no position of its own.
    let names: HashSet<String> = forwarders.keys().cloned().collect();
    let mut in_initializers = HashSet::new();
    for global in &module.globals {
        inline::collect_func_refs_from_initializer(&global.init, &names, &mut in_initializers);
    }
    for (name, pack) in forwarders {
        if in_initializers.contains(name) {
            found.entry(name.as_str()).or_insert(pack.pos);
        }
    }

    found
        .into_iter()
        .map(|(name, pos)| ForwarderCopy {
            function: name.to_string(),
            pack: forwarders[name],
            pos,
            why: CopyDemand::AddressTaken,
        })
        .collect()
}

/// Report each forwarder that would need a copy. GCC rejects the same
/// programs: "invalid use of '__builtin_va_arg_pack ()'".
fn report_forwarder_copies(copies: &[ForwarderCopy]) {
    for copy in copies {
        let template = match copy.why {
            CopyDemand::ExternalDefinition => {
                "invalid use of '{0} ()': '{1}' forwards its caller's arguments, so it cannot be an external definition"
            }
            CopyDemand::AddressTaken => {
                "invalid use of '{0} ()': '{1}' forwards its caller's arguments, so its address cannot be taken"
            }
        };
        crate::diag::error_args(copy.pos, template, &[copy.pack.builtin, &copy.function]);
    }
}

/// Keep a forwarding function's body out of the object file.
///
/// The parser has already required it to be `always_inline`, so every call is
/// substituted; the standalone copy would have nothing to forward.
fn suppress_forwarding_bodies(module: &mut Module) {
    for func in &mut module.functions {
        if forwards_caller_arguments(func) {
            func.emit = false;
        }
    }
}

/// Diagnose a pack the inliner could not resolve.
///
/// Reachable when `always_inline` is refused for a reason of its own -- a
/// callee that also uses `va_start`, or `alloca`, is turned down in
/// `ir::inline` before the attribute is consulted. The body is already
/// suppressed by then, so what is wrong is the surviving *call site*, and that
/// is what this looks for. GCC reports the same situation as an error.
fn check_forwarding_resolved(module: &Module) {
    // Two kinds of function have no out-of-line copy to fall back on. A
    // `__builtin_va_arg_pack` forwarder is suppressed on the assumption it
    // always inlines. And an inline definition marked `always_inline` that no
    // call site *can* substitute -- it reads its own variadic frame, is
    // recursive, takes a label's address, returns an address, or is also
    // marked `noinline` -- is a contradiction gcc rejects too.
    //
    // A plain C99 inline definition is deliberately not in this set: its
    // external definition may live in another translation unit, so an ordinary
    // call to it is correct and the linker resolves it.
    //
    // Neither is an `always_inline` function the inliner merely *declined*.
    // Its caps on caller size and on recursive stack depth are c17's own, and
    // gcc has no counterpart: treating those refusals as unresolvable rejected
    // programs gcc compiles, which any recursive function over a few hundred
    // instructions calling a glibc `__fortify_function` reached. The call is
    // left standing and the linker resolves it against the out-of-line
    // definition the inline definition promises.
    let impossible = crate::ir::inline::impossible_always_inline(module);
    let suppressed: std::collections::BTreeMap<&str, bool> = module
        .functions
        .iter()
        .filter(|f| !f.emit && (forwards_caller_arguments(f) || impossible.contains(&f.name)))
        .map(|f| (f.name.as_str(), forwards_caller_arguments(f)))
        .collect();
    if suppressed.is_empty() {
        return;
    }

    // One report per unresolved callee, however many times it is called.
    let mut reported = std::collections::BTreeSet::new();
    for func in module.functions.iter().filter(|f| f.emit) {
        for insn in func.blocks.iter().flat_map(|b| &b.insns) {
            // A `__builtin_X` call reaches the library's `X`, which has the
            // out-of-line definition this one lacks.
            let Some(callee) = insn.local_callee() else {
                continue;
            };
            let Some(&forwards) = suppressed.get(callee) else {
                continue;
            };
            if reported.insert(callee) {
                // The call site, not line 0: it is the thing that cannot be
                // resolved, and the only position either function still has.
                if forwards {
                    crate::diag::error_args(
                        insn.pos.unwrap_or_default(),
                        "'__builtin_va_arg_pack' in '{0}' could not be forwarded: the function was not inlined",
                        &[callee],
                    );
                } else {
                    crate::diag::error_args(
                        insn.pos.unwrap_or_default(),
                        "inlining failed in call to 'always_inline' '{0}', which has no out-of-line definition",
                        &[callee],
                    );
                }
            }
        }
    }
}

/// Optimize a module as `opt` asks.
///
/// Level 0: `__attribute__((always_inline))` inlining, then `memexpand`
/// Level 1+: inlining and `memexpand`, then the per-function passes below to
/// fixed point
pub fn optimize_module(
    module: &mut Module,
    types: &TypeTable,
    opt: Optimization,
    target: &Target,
) -> OptReport {
    // Phase 1: Function inlining (module-level pass)
    // This inlines small functions at their call sites and removes
    // dead static functions that were fully inlined.
    //
    // Runs even at -O0, where it admits only `__attribute__((always_inline))`
    // functions -- gcc honours that attribute with optimization off. It is a
    // no-op for a module that has none.
    // A function that forwards its caller's variadic arguments has no
    // out-of-line form: what it forwards exists only at a call site. GCC emits
    // no standalone copy of one either, and rejects a program that needs one.
    report_forwarder_copies(&external_forwarders(module));
    let forwarders = static_forwarders(module);
    suppress_forwarding_bodies(module);

    if opt.optimizes() {
        for func in &mut module.functions {
            simplify_before_inlining(func, types);
        }
    }
    inline::run(module, opt);

    // Every pack should have been resolved by the splice above. One that
    // survives would be emitted as a call with its arguments missing, so say
    // so instead.
    check_forwarding_resolved(module);

    // A `memcpy`, `memset` or `memmove` of a small constant length becomes
    // loads and stores at every level, as the linearizer's own aggregate
    // copies already are: nothing about it is an optimization a debugger
    // would miss. After inlining, which is what makes some lengths constant.
    for func in &mut module.functions {
        memexpand::run(func, types);
    }

    let report = if opt.optimizes() {
        optimize_functions(module, types, target, MAX_ITERATIONS)
    } else {
        OptReport::default()
    };

    // An immediate-only asm operand must be a constant by now, which is when
    // gcc decides too: inlining a literal into `"i"(param)` satisfies it at
    // -O2 and nothing does at -O0. The copies and address arithmetic that
    // carried the constant are dead once the operand names it.
    let thread_locals = asm_operand::thread_locals(module);
    for func in &mut module.functions {
        if asm_operand::resolve_immediates(func, &thread_locals) && opt.optimizes() {
            dce::run(func);
            func.remove_nops();
        }
    }

    // An address that survived everything above would reach the link as an
    // undefined reference to the suppressed body.
    report_forwarder_copies(&forwarder_addresses(module, &forwarders));
    report
}

/// Run the per-function passes over every function, each for at most
/// `max_iterations` rounds.
fn optimize_functions(
    module: &mut Module,
    types: &TypeTable,
    target: &Target,
    max_iterations: usize,
) -> OptReport {
    // Module-wide facts the passes need. Built after inlining, so the call
    // graph and the set of globals are final.
    let known = constglobal::KnownGlobals::collect(module, types);
    let mi = memloc::ModuleInfo::build(module, types);
    let sizes = ObjectSizes::build(module, types);
    let (functions, strings, callees) = module.split_for_rewrite();
    let literals = libcall_fold::NewLiterals::new(strings);
    let fold = libcall_fold::FoldCtx {
        types,
        target,
        mi: &mi,
        bytes: known.bytes(),
        callees,
        literals: &literals,
    };
    let ctx = PassCtx {
        types,
        known: &known,
        mi: &mi,
        fold: &fold,
        sizes: &sizes,
    };
    let mut report = OptReport::default();
    for func in functions {
        let c = optimize_function(func, &ctx, max_iterations);
        if !c.converged() {
            report.unconverged.push(c);
        }
        // A local whose every access was forwarded or deleted needs no slot.
        mem2reg(func);
    }
    report
}

/// Optimize a single function by running `PASSES` until none changes it, or
/// for `max_iterations` rounds.
///
/// A `__builtin_object_size` whose object is still unknown when nothing
/// changes any more is unknown for good, and is answered so -- `(size_t)-1`
/// or 0 -- after which the passes run to a fixed point again: the answer is
/// what lets `libcall_fold` turn a `_chk` call of an unknown size into the
/// plain call, which may fold in turn.
fn optimize_function(func: &mut Function, ctx: &PassCtx, max_iterations: usize) -> Convergence {
    let mut c = Convergence {
        function: func.name.clone(),
        iterations: 0,
        changes: [0; PASSES.len()],
        still_changing: Vec::new(),
    };
    run_passes(func, ctx, max_iterations, &mut c);
    if objsize::run(func, ctx.types, ctx.sizes, Settle::Everything) {
        run_passes(func, ctx, max_iterations, &mut c);
    }
    c
}

/// Run `PASSES` over `func` until none changes it, or for `max_iterations`
/// rounds, recording how it went in `c`.
fn run_passes(func: &mut Function, ctx: &PassCtx, max_iterations: usize, c: &mut Convergence) {
    for _ in 0..max_iterations {
        c.iterations += 1;
        c.still_changing.clear();
        for (i, (name, run)) in PASSES.iter().enumerate() {
            if run(func, ctx) {
                c.changes[i] += 1;
                c.still_changing.push(name);
            }
        }
        // Every pass skips the `Nop`s the others leave, and they only
        // accumulate; nothing refers to an instruction by position across
        // passes, so they can go.
        func.remove_nops();
        if c.still_changing.is_empty() {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Opcode, Pseudo, PseudoId};

    /// `entry: cbr 1, .L1, .L2`, each arm returning: one round folds the
    /// branch and deletes the dead arm, and a second finds nothing left.
    fn module_with_a_constant_branch(types: &TypeTable) -> Module {
        let mut func = Function::new("f", types.int_id);
        func.add_pseudo(Pseudo::val(PseudoId(1), 1));
        func.next_pseudo = 2;
        let (entry, then, els) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        let mut b0 = BasicBlock::new(entry);
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::cbr(PseudoId(1), then, els));
        b0.children = vec![then, els];
        func.add_block(b0);
        for id in [then, els] {
            let mut b = BasicBlock::new(id);
            b.add_insn(Instruction::ret(None));
            b.parents = vec![entry];
            func.add_block(b);
        }
        func.entry = entry;
        let mut module = Module::default();
        module.functions.push(func);
        module
    }

    fn at_line(line: u32) -> Position {
        Position {
            line,
            ..Default::default()
        }
    }

    /// A function whose single block holds `insns` and returns.
    fn function_of(name: &str, types: &TypeTable, insns: Vec<Instruction>) -> Function {
        let mut func = Function::new(name, types.int_id);
        let mut b = BasicBlock::new(BasicBlockId(0));
        b.add_insn(Instruction::new(Opcode::Entry));
        for insn in insns {
            b.add_insn(insn);
        }
        b.add_insn(Instruction::ret(None));
        func.add_block(b);
        func.entry = BasicBlockId(0);
        func
    }

    /// `static int count(const char *, ...)` using
    /// `__builtin_va_arg_pack_len()` on line 2, and `main`, made of `body`.
    fn module_with_forwarder(types: &TypeTable, body: Vec<Instruction>) -> Module {
        let mut count = function_of(
            "count",
            types,
            vec![Instruction::new(Opcode::VaArgPackLen)
                .with_target(PseudoId(1))
                .with_pos(at_line(2))],
        );
        count.is_static = true;
        let mut main = function_of("main", types, body);
        main.add_pseudo(Pseudo::sym(PseudoId(1), "count".to_string()));
        main.next_pseudo = 3;
        let mut module = Module::default();
        module.functions.extend([count, main]);
        module
    }

    /// The resume point of a `__builtin_setjmp` is inside the instruction,
    /// so the optimizer sees an ordinary instruction whose result it cannot
    /// know: at -O2 the setjmp survives as the builtin, and so do both arms
    /// of the branch on its result -- the one only a `__builtin_longjmp`
    /// reaches included.
    #[test]
    fn builtin_setjmp_and_its_resume_arm_survive_optimization() {
        let src = r#"
void *buf[5];
extern void g(void);
int f(void) { if (__builtin_setjmp(buf)) return 7; g(); return 0; }
"#;
        for target in [
            Target::new(crate::target::Arch::X86_64, crate::target::Os::Linux),
            Target::new(crate::target::Arch::Aarch64, crate::target::Os::Linux),
        ] {
            let (mut module, types) =
                crate::ir::linearize::test_linearize::linearize_source_with_types(src, &target);
            let opt = Optimization::from_flag("2").expect("valid level");
            optimize_module(&mut module, &types, opt, &target);
            let f = module.functions.iter().find(|f| f.name == "f").unwrap();
            assert!(f.receives_nonlocal_goto(), "the builtin setjmp was removed");
            let insns: Vec<&Instruction> = f.blocks.iter().flat_map(|b| &b.insns).collect();
            assert!(
                insns.iter().any(|i| i.local_callee() == Some("g")),
                "the direct arm was removed"
            );
            let sevens = f
                .pseudos
                .iter()
                .any(|p| matches!(p.kind, crate::ir::PseudoKind::Val(7)));
            assert!(sevens, "the resume arm's `return 7` was removed");
        }
    }

    /// A taken address is found where it is taken, a call is not an address,
    /// and an address in an initializer falls back to the builtin's position.
    #[test]
    fn forwarder_addresses_are_found_where_taken() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let pack = PackUse {
            builtin: "__builtin_va_arg_pack_len",
            pos: at_line(2),
        };

        let mut taken = module_with_forwarder(
            &types,
            vec![
                Instruction::sym_addr(PseudoId(2), PseudoId(1), types.int_id).with_pos(at_line(5)),
            ],
        );
        let forwarders = static_forwarders(&taken);
        assert_eq!(forwarders.get("count"), Some(&pack));
        assert_eq!(
            forwarder_addresses(&taken, &forwarders),
            vec![ForwarderCopy {
                function: "count".to_string(),
                pack,
                pos: at_line(5),
                why: CopyDemand::AddressTaken,
            }]
        );

        let called = module_with_forwarder(
            &types,
            vec![Instruction::call(
                None,
                "count",
                vec![],
                vec![],
                types.int_id,
                32,
            )],
        );
        assert!(forwarder_addresses(&called, &static_forwarders(&called)).is_empty());

        let mut tabled = module_with_forwarder(&types, vec![]);
        tabled.globals.push(crate::ir::GlobalDef::new(
            "tbl",
            types.int_id,
            crate::ir::Initializer::SymAddr("count".to_string()),
        ));
        let copies = forwarder_addresses(&tabled, &static_forwarders(&tabled));
        assert_eq!(copies.len(), 1);
        assert_eq!(copies[0].pos, at_line(2));

        // Taken only inside a function that is not emitted: nothing calls it.
        taken.functions[1].emit = false;
        assert!(forwarder_addresses(&taken, &forwarders).is_empty());

        // `int use(int count) { sink(&count); }`: the address of a parameter
        // spelled like the forwarder, which is not the forwarder.
        let mut shadowed = module_with_forwarder(
            &types,
            vec![Instruction::sym_addr(
                PseudoId(2),
                PseudoId(1),
                types.int_id,
            )],
        );
        shadowed.functions[1].add_local("count", PseudoId(1), types.int_id, None, None);
        assert!(forwarder_addresses(&shadowed, &static_forwarders(&shadowed)).is_empty());
    }

    /// Only an emitted, non-`static` forwarder is an external definition; an
    /// inline definition leaves that to another translation unit.
    #[test]
    fn external_forwarders_are_the_emitted_external_ones() {
        let target = Target::host();
        let types = TypeTable::new(&target);
        let mut module = module_with_forwarder(&types, vec![]);
        assert!(external_forwarders(&module).is_empty(), "static");

        module.functions[0].is_static = false;
        let copies = external_forwarders(&module);
        assert_eq!(copies.len(), 1);
        assert_eq!(copies[0].why, CopyDemand::ExternalDefinition);
        assert_eq!(copies[0].pos, at_line(2));
        assert!(static_forwarders(&module).is_empty());

        module.functions[0].emit = false;
        assert!(external_forwarders(&module).is_empty(), "inline definition");
    }

    /// A loop cut off by its iteration cap says so, and names what was still
    /// moving; one that reaches its fixed point is not reported at all.
    #[test]
    fn optimizer_reports_a_function_it_could_not_finish() {
        let target = Target::host();
        let types = TypeTable::new(&target);

        let mut cut_short = module_with_a_constant_branch(&types);
        let report = optimize_functions(&mut cut_short, &types, &target, 1);
        let [c] = report.unconverged.as_slice() else {
            panic!("one function cut short, got {:?}", report.unconverged);
        };
        assert_eq!(c.function, "f");
        assert_eq!(c.iterations, 1);
        assert!(!c.converged());
        assert!(
            c.still_changing.contains(&"dce"),
            "the dead arm was deleted in the last round: {:?}",
            c.still_changing
        );
        let total: u32 = c.changes.iter().sum();
        assert_eq!(total as usize, c.still_changing.len());

        let mut finished = module_with_a_constant_branch(&types);
        let report = optimize_functions(&mut finished, &types, &target, MAX_ITERATIONS);
        assert!(report.unconverged.is_empty(), "{:?}", report.unconverged);
        // The dead arm is gone, and the live one merged into the entry.
        assert_eq!(finished.functions[0].blocks.len(), 1);
    }

    /// The spellings GCC and Clang accept, and what each means here.
    #[test]
    fn optimization_parses_every_supported_spelling() {
        for (flag, optimizes, aggressive) in [
            ("0", false, false),
            ("1", true, false),
            ("2", true, true),
            ("3", true, true),
            // -Og is -O1 minus the debugger-hostile passes; c17 has none to
            // drop, so only the level carries over.
            ("g", true, false),
            // -Os is -O2 without the size-increasing choices.
            ("s", true, true),
        ] {
            let opt = Optimization::from_flag(flag)
                .unwrap_or_else(|e| panic!("-O{flag} should parse: {e}"));
            assert_eq!(opt.optimizes(), optimizes, "-O{flag} optimizes()");
            assert_eq!(
                opt.inlines_aggressively(),
                aggressive,
                "-O{flag} inlines_aggressively()"
            );
        }

        // `-Ofast` and `-Oz` name an extra c17 does not have, not a level it
        // cannot reach. Each takes the nearest level it does have and says so
        // on stderr; refusing them failed builds over a flag that could only
        // ever have bought speed.
        let fast = Optimization::from_flag("fast").expect("-Ofast must be accepted");
        assert_eq!(fast, Optimization::from_flag("3").unwrap());
        let oz = Optimization::from_flag("z").expect("-Oz must be accepted");
        assert_eq!(oz, Optimization::from_flag("s").unwrap());

        // A level that names nothing is still refused, by name rather than by
        // clap's "invalid digit found in string".
        assert!(Optimization::from_flag("9").is_err());
        assert!(Optimization::from_flag("").is_err());

        // No -O at all is no optimization.
        assert!(!Optimization::default().optimizes());
    }

    /// `-fno-inline` is orthogonal to the level: it stops general inlining
    /// without stopping optimization, which is why GCC defines `__OPTIMIZE__`
    /// and `__NO_INLINE__` together for `-O2 -fno-inline`.
    #[test]
    fn optimization_separates_inlining_from_the_level() {
        let mut o2 = Optimization::from_flag("2").unwrap();
        assert!(o2.optimizes() && o2.inlines_generally());

        o2.set_inlining(false);
        assert!(o2.optimizes(), "-fno-inline must not stop optimization");
        assert!(!o2.inlines_generally());
        assert!(o2.inlines_aggressively(), "still level 2");

        // Last one wins.
        o2.set_inlining(true);
        assert!(o2.inlines_generally());

        // At -O0 nothing is inlined generally, with or without the flag.
        let mut o0 = Optimization::from_flag("0").unwrap();
        assert!(!o0.inlines_generally());
        o0.set_inlining(true);
        assert!(
            !o0.inlines_generally(),
            "-finline does not turn on inlining at -O0, as in GCC"
        );
    }
}
