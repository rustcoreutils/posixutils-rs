//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The exception-handling tables of `-fexceptions`: gcc's LSDA and the
// personality routine that reads it, for the calls that have a landing pad
// (`ir::InsnExtra::landing_pad`).
//

//! How an unwind finds a function's landing pads.
//!
//! The unwinder walks the frames with the CFI. For a procedure whose FDE
//! names a personality routine (`.cfi_personality`), it calls that routine
//! with the procedure's LSDA (`.cfi_lsda`), which says where control goes
//! when the unwind passes each call. C has only cleanups, so gcc's C
//! personality, `__gcc_personality_v0`, reads the simplest LSDA there is: no
//! landing-pad base and no type table, then a table of call sites, each the
//! call's extent and its landing pad as offsets from the function's start,
//! and an action of 0, "cleanup". A call in no entry has no landing pad, and
//! the unwind passes it by, so only the calls with a pad are listed.
//!
//! The pad runs with the stack pointer the unwinder computes from the CFA,
//! which is the one at the call. c17 reserves a call's stacked arguments
//! below the frame and releases them after the call, so the pad would start
//! that much lower than the code around it; `.cfi_GNU_args_size` tells the
//! unwinder to add it back, as gcc does for pushed arguments.
//!
//! The personality routine is named PC-relative through a pointer the
//! linker resolves, `DW.ref.__gcc_personality_v0`: a hidden weak object in a
//! COMDAT group, one per output, so that position-independent code needs no
//! dynamic relocation against the routine in `.eh_frame`. Absolute code
//! names the routine directly. Mach-O's assembler makes the indirection
//! itself.

use super::codegen::CodeGenBase;
use super::lir::{internal_label, Directive, EmitAsm, Label, LirInst, Symbol, SymbolType};
use crate::ir::{BasicBlockId, Function, Instruction};
use crate::target::Os;

/// gcc's personality routine for C.
const PERSONALITY: &str = "__gcc_personality_v0";

/// The PC-relative pointer to [`PERSONALITY`] that position-independent
/// ELF code names in its FDEs.
const PERSONALITY_REF: &str = "DW.ref.__gcc_personality_v0";

/// `DW_EH_PE_indirect | DW_EH_PE_pcrel | DW_EH_PE_sdata4`.
const INDIRECT_PCREL_SDATA4: u8 = 0x9b;
/// `DW_EH_PE_pcrel | DW_EH_PE_sdata4`.
const PCREL_SDATA4: u8 = 0x1b;
/// `DW_EH_PE_udata4`, for absolute code.
const UDATA4: u8 = 0x03;
/// `DW_EH_PE_pcrel`, the LSDA encoding Apple's assembler takes.
const PCREL: u8 = 0x10;
/// `DW_EH_PE_omit`.
const OMIT: i64 = 0xff;
/// `DW_EH_PE_uleb128`.
const ULEB128: i64 = 0x01;

/// One call that has a landing pad: the labels around the call instruction,
/// and the pad's.
pub struct CallSite {
    begin: Symbol,
    end: Symbol,
    pad: Symbol,
}

/// One function's LSDA.
struct Lsda {
    /// The label `.cfi_lsda` names.
    label: Symbol,
    /// The function's first byte, where its FDE starts.
    start: Symbol,
    sites: Vec<CallSite>,
}

/// The unit's exception-handling tables, gathered as the functions are
/// emitted and written once they all are.
#[derive(Default)]
pub struct EhTables {
    /// The LSDA of the function being emitted, if it has one.
    current: Option<Lsda>,
    /// The `.cfi_GNU_args_size` in force in the function being emitted.
    args_size: i64,
    /// Every function's, to be written at the end of the unit.
    done: Vec<Lsda>,
    /// The next number for a label of these tables.
    next_label: u32,
}

impl EhTables {
    fn label(&mut self, what: &str) -> Symbol {
        let n = self.next_label;
        self.next_label += 1;
        Symbol::local(internal_label(what, n))
    }
}

/// Whether `func` has a call with a landing pad, and so needs an LSDA.
pub fn has_landing_pads(func: &Function) -> bool {
    func.blocks
        .iter()
        .flat_map(|b| &b.insns)
        .any(|i| i.extra().landing_pad.is_some())
}

impl<I: LirInst + EmitAsm> CodeGenBase<I> {
    /// At `.cfi_startproc` of `func`: if it has landing pads, mark its start
    /// and name the personality routine and its LSDA. `pic` is whether the
    /// code is position independent.
    pub fn eh_begin_function(&mut self, func: &Function, pic: bool) {
        self.eh.args_size = 0;
        if !self.cfi.emitted() || !has_landing_pads(func) {
            return;
        }
        let start = self.eh.label("LFB");
        let label = self.eh.label("LLSDA");
        self.push_directive(Directive::GlobalLabel(start.clone()));
        let (personality, lsda) = match (self.target.os, pic) {
            (Os::MacOS, _) => (
                (INDIRECT_PCREL_SDATA4, Symbol::extern_sym(PERSONALITY)),
                PCREL,
            ),
            (_, true) => (
                (INDIRECT_PCREL_SDATA4, Symbol::global(PERSONALITY_REF)),
                PCREL_SDATA4,
            ),
            (_, false) => ((UDATA4, Symbol::extern_sym(PERSONALITY)), UDATA4),
        };
        self.push_directive(Directive::CfiPersonality {
            encoding: personality.0,
            sym: personality.1,
        });
        self.push_directive(Directive::CfiLsda {
            encoding: lsda,
            sym: label.clone(),
        });
        self.eh.current = Some(Lsda {
            label,
            start,
            sites: Vec::new(),
        });
    }

    /// Just before the call instruction of `insn`, with `args_size` bytes
    /// of stacked arguments below the frame: its landing pad, if it has
    /// one, with the label its call-site entry starts at.
    pub fn eh_call_begin(&mut self, insn: &Instruction, args_size: i64) -> Option<CallSite> {
        let pad = insn.extra().landing_pad?;
        self.eh.current.as_ref()?;
        if args_size != self.eh.args_size {
            self.push_directive(Directive::CfiGnuArgsSize(args_size));
            self.eh.args_size = args_size;
        }
        let begin = self.eh.label("LEHB");
        let end = self.eh.label("LEHE");
        self.push_directive(Directive::GlobalLabel(begin.clone()));
        Some(CallSite {
            begin,
            end,
            pad: self.landing_pad_label(pad),
        })
    }

    /// Just after the call instruction `eh_call_begin` answered `site` for.
    pub fn eh_call_end(&mut self, site: Option<CallSite>) {
        let Some(site) = site else {
            return;
        };
        self.push_directive(Directive::GlobalLabel(site.end.clone()));
        if let Some(lsda) = &mut self.eh.current {
            lsda.sites.push(site);
        }
    }

    /// The label of block `pad` of the function being emitted.
    fn landing_pad_label(&self, pad: BasicBlockId) -> Symbol {
        Label::block(&self.current_fn, pad.0).symbol()
    }

    /// At `.cfi_endproc`: the function's LSDA, if any, waits for the end of
    /// the unit.
    pub fn eh_end_function(&mut self) {
        if let Some(lsda) = self.eh.current.take() {
            self.eh.done.push(lsda);
        }
    }

    /// At the end of the unit: every function's LSDA in the exception
    /// table section, and the personality pointer they name. `pic` as for
    /// [`Self::eh_begin_function`].
    pub fn emit_eh_tables(&mut self, pic: bool) {
        let tables = std::mem::take(&mut self.eh.done);
        if tables.is_empty() {
            return;
        }
        self.push_directive(Directive::GccExceptTable);
        for lsda in tables {
            self.push_directive(Directive::Align(2));
            self.push_directive(Directive::GlobalLabel(lsda.label));
            // @LPStart: the function's start. @TType: no type table.
            self.push_directive(Directive::Byte(OMIT));
            self.push_directive(Directive::Byte(OMIT));
            self.push_directive(Directive::Byte(ULEB128));
            let table = self.eh.label("LLSDACSB");
            let table_end = self.eh.label("LLSDACSE");
            self.push_directive(Directive::Uleb128Diff {
                end: table_end.clone(),
                start: table.clone(),
            });
            self.push_directive(Directive::GlobalLabel(table));
            for site in lsda.sites {
                for (end, start) in [
                    (&site.begin, &lsda.start),
                    (&site.end, &site.begin),
                    (&site.pad, &lsda.start),
                ] {
                    self.push_directive(Directive::Uleb128Diff {
                        end: end.clone(),
                        start: start.clone(),
                    });
                }
                // Action 0: a cleanup, which catches nothing.
                self.push_directive(Directive::Uleb128(0));
            }
            self.push_directive(Directive::GlobalLabel(table_end));
        }
        if pic && self.target.os != Os::MacOS {
            self.emit_personality_ref();
        }
    }

    /// `DW.ref.__gcc_personality_v0`, the pointer to the personality routine
    /// each object defines and the linker keeps one of.
    fn emit_personality_ref(&mut self) {
        let sym = Symbol::global(PERSONALITY_REF);
        self.push_directive(Directive::Visibility(sym.clone(), "hidden".into()));
        self.push_directive(Directive::Weak(
            sym.clone(),
            super::lir::WeakKind::Definition,
        ));
        self.push_directive(Directive::ComdatData(sym.clone()));
        self.push_directive(Directive::Align(3));
        self.push_directive(Directive::Type {
            sym: sym.clone(),
            kind: SymbolType::Object,
        });
        self.push_directive(Directive::Size {
            sym: sym.clone(),
            size: 8,
        });
        self.push_directive(Directive::GlobalLabel(sym));
        self.push_directive(Directive::QuadSym(Symbol::extern_sym(PERSONALITY)));
    }
}
