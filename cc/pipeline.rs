//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The compilation of one translation unit: source bytes to tokens, and
// preprocessed tokens to assembly text.
//
// The driver and the in-process tests both run it, so a test that reads the
// diagnostics or the assembly of a translation unit reads what `c17` itself
// would have produced from the same pipeline, not a copy of it.
//

use crate::arch;
use crate::diag;
use crate::ir;
use crate::opt;
use crate::parse::{self, ast::TranslationUnit, Parser};
use crate::prefix_map::PrefixMap;
use crate::strings::StringTable;
use crate::symbol::SymbolTable;
use crate::target::{self, Target};
use crate::token::{self, lexer::Token, replace_trigraphs, strip_bom, Tokenizer};
use crate::types::TypeTable;
use std::borrow::Cow;
use std::io;

/// Translation phases 1 and 2 and tokenization of one source file, and the
/// stream it was registered as.
///
/// `preprocessed` is a `.i` operand, whose phases 1 and 2 already ran (POSIX
/// 87981-87983) and must not be repeated.
pub fn source_tokens(
    buffer: &[u8],
    display_path: &str,
    trigraphs: bool,
    preprocessed: bool,
    strings: &mut StringTable,
) -> (Vec<Token>, u16) {
    // Phase 0, before anything looks at the bytes: a byte order mark is not
    // part of the program. Left in place it lexes as an identifier character,
    // so the first line is never a directive.
    let buffer = strip_bom(buffer);

    // Translation phase 1, before anything else looks at the bytes.
    let buffer = if trigraphs && !preprocessed {
        replace_trigraphs(buffer)
    } else {
        std::borrow::Cow::Borrowed(buffer)
    };

    let stream_id = diag::init_stream(display_path);
    let mut tokenizer = Tokenizer::new(&buffer, stream_id, strings);
    // Translation phase 2 likewise already ran.
    if preprocessed {
        tokenizer = tokenizer.without_splicing();
    }
    (tokenizer.tokenize(), stream_id)
}

/// How the parsed translation unit becomes assembly.
pub struct CodegenOptions<'a> {
    pub optimization: opt::Optimization,
    /// `-fmath-errno` (the default) rather than `-fno-math-errno`.
    pub math_errno: bool,
    /// `-g`.
    pub debug: bool,
    /// `-ftrapping-math` (the default) rather than `-fno-trapping-math`.
    pub trapping_math: bool,
    /// `-fvisibility=`.
    pub default_visibility: Option<&'a str>,
    /// Code for a shared object (`-shared` or `-fPIC`): selects the TLS model.
    pub shared_mode: bool,
    /// Position-independent code.
    pub pic: bool,
    pub unwind_tables: bool,
    /// `-fverbose-asm`.
    pub verbose_asm: bool,
    /// The name DWARF records for the primary source file.
    pub source_name: &'a str,
    /// `-fdebug-prefix-map`: rewrites every path the debug information and
    /// the `.file` directives record.
    pub debug_prefix_map: &'a PrefixMap,
}

/// The pipeline's points of interest to the driver: its dumps, `--stats`, and
/// the places a dump option stops compilation early.
pub trait Observer {
    /// The translation unit parsed without error. `Ok(false)` stops here
    /// without failing; `Err` fails.
    fn parsed(&mut self, _ast: &TranslationUnit) -> io::Result<bool> {
        Ok(true)
    }

    /// The module was just linearized.
    fn linearized(
        &mut self,
        _module: &ir::Module,
        _strings: &StringTable,
        _types: &TypeTable,
        _symbols: &SymbolTable,
    ) {
    }

    /// The module reached the named stage. `report` is the optimizer's, at
    /// `post-opt` only. Returns whether to go on.
    fn stage(
        &mut self,
        _stage: &str,
        _module: &ir::Module,
        _types: &TypeTable,
        _report: Option<&opt::OptReport>,
    ) -> bool {
        true
    }
}

/// An observer that changes nothing.
pub struct Quiet;

impl Observer for Quiet {}

fn failed(what: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, what.to_string())
}

/// Compile preprocessed tokens to assembly text.
///
/// `Ok(None)` is an observer stopping early. Every diagnostic is reported
/// through [`diag`]; an `Err` means at least one was an error, or the parse
/// could not continue.
pub fn compile_tokens(
    mut preprocessed: Vec<Token>,
    strings: &StringTable,
    target: &Target,
    opts: &CodegenOptions,
    observer: &mut dyn Observer,
) -> io::Result<Option<String>> {
    // Create symbol table and type table BEFORE parsing
    // symbols are bound during parsing
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(target);

    // Pull the pragma markers out of the stream and note where they stood.
    // Done here, on the finished token vector, because that is the first
    // point at which the order is the translation unit's own -- an include is
    // preprocessed separately and spliced in, so nothing recorded earlier
    // survives with a usable index.
    let pack_directives = token::preprocess::extract_pragma_directives(&mut preprocessed);

    // Parse (this also binds symbols to the symbol table)
    let mut parser = Parser::new(
        &preprocessed,
        strings,
        &mut symbols,
        &mut types,
        pack_directives,
    );
    parser.set_library_call_policy(parse::LibraryCallPolicy {
        optimizing: opts.optimization.optimizes(),
        math_errno: opts.math_errno,
    });
    let ast = parser
        .parse_translation_unit()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {}", e)))?;

    // Check for semantic errors (e.g., undeclared identifiers) reported during parsing
    if diag::has_error() != 0 {
        return Err(failed("compilation failed"));
    }

    if !observer.parsed(&ast)? {
        return Ok(None);
    }

    // Linearize to IR
    let mut module = ir::linearize::linearize(
        &ast,
        &symbols,
        &types,
        strings,
        target,
        opts.debug,
        opts.trapping_math,
    );
    if let Some(how) = opts.default_visibility {
        module.apply_default_visibility(how);
    }

    // Check for errors during linearization (e.g., unsupported global initializers)
    if diag::has_error() != 0 {
        return Err(failed("compilation failed"));
    }

    observer.linearized(&module, strings, &types, &symbols);

    // Set DWARF metadata. Every path it records goes through the debug
    // prefix map, as gcc's does: the compilation directory, the primary
    // source's name, and each `.file`. A placeholder for a synthetic stream
    // is not a path and stays empty.
    let map = opts.debug_prefix_map;
    module.source_name = Some(map.apply(opts.source_name).into_owned());
    module.comp_dir = std::env::current_dir()
        .ok()
        .map(|p| map.apply(&p.to_string_lossy()).into_owned());
    for file in module.source_files.iter_mut().filter(|f| !f.is_empty()) {
        if let Cow::Owned(mapped) = map.apply(file) {
            *file = mapped;
        }
    }

    observer.stage("post-linearize", &module, &types, None);
    ir::validate::verify(&module, ir::validate::Stage::Ssa, "linearization");

    // A `destructor` on Mach-O is an `atexit` registration rather than a
    // table entry; see `ir::mach_o_dtors`. Runs before mapping so the calls it
    // synthesizes are classified with every other call.
    ir::mach_o_dtors::register_destructors_with_atexit(
        &mut module,
        &types,
        target,
        target.os == target::Os::MacOS,
    );

    // Hardware mapping pass — centralized target-specific lowering decisions
    arch::mapping::run_mapping(&mut module, &types, target);

    observer.stage("post-mapping", &module, &types, None);

    // Expand thread-local accesses for the dynamic TLS model. Must run before
    // `optimize_module`, because register allocation is downstream of it and
    // has to see the address computation -- see `ir::tls`. The backend below
    // is given the same `shared_mode`, so the pass and the backend agree on
    // the model.
    ir::tls::expand_dynamic_tls(
        &mut module,
        target.tls_access(opts.shared_mode).is_call(),
        &types,
    );

    observer.stage("post-tls", &module, &types, None);
    ir::validate::verify(&module, ir::validate::Stage::Ssa, "target mapping");

    // Optimize IR. Called even at -O0, where the only pass that does anything
    // is inlining of `__attribute__((always_inline))` functions, which gcc
    // honours with optimization off.
    let report = opt::optimize_module(&mut module, &types, opts.optimization, target);

    // An opcode the target computes by a library call -- a libm function it
    // has no instruction for, any binary128 operation, or an x86-64
    // `_Float16` one -- becomes that call after the optimizer, which could
    // still fold it.
    arch::mapping::call_library_fallbacks(&mut module, &types, target);
    ir::validate::verify(&module, ir::validate::Stage::Ssa, "optimization");

    if !observer.stage("post-opt", &module, &types, Some(&report)) {
        return Ok(None);
    }

    // Lower IR (phi elimination, etc.)
    ir::lower::lower_module(&mut module);

    if !observer.stage("post-lower", &module, &types, None) {
        return Ok(None);
    }

    // Generate assembly. `shared_mode` selects the TLS model and nothing
    // else. `-fPIC` asks for code that can live in a shared object, which is
    // exactly what Local Exec cannot satisfy, while `-fPIE` and the PIE
    // default do not, because a PIE executable still resolves its own
    // thread-locals at link time. gcc draws the line in the same place.
    let mut codegen = arch::codegen::create_codegen(
        target.clone(),
        opts.unwind_tables,
        opts.pic,
        opts.shared_mode,
        opts.verbose_asm,
    );
    let asm = codegen.generate(&module, &types);

    // Codegen can diagnose too. Inline asm is the case that reaches here: a
    // constraint's register class is only confronted with the operand's actual
    // location once registers are allocated, so "memory input 0 is not
    // directly addressable" cannot be raised any earlier. Without this
    // checkpoint the error would be printed and the broken object written
    // anyway.
    if diag::has_error() != 0 {
        return Err(failed("compilation failed"));
    }

    Ok(Some(asm))
}
