//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// c17 - A POSIX C99 compiler
//

#![recursion_limit = "512"]

mod abi;
mod arch;
mod builtin_headers;
mod builtins;
mod diag;
mod ir;
mod kw;
mod linkargs;
mod opt;
mod os;
mod parse;
mod rtlib;
mod strings;
mod symbol;
mod target;
mod token;
mod types;

use clap::Parser;
use gettextrs::{
    bind_textdomain_codeset, gettext, gettext_args, setlocale, textdomain, LocaleCategory,
};
use std::fs::File;
use std::io::{self, BufReader, Read, Write};
use std::path::Path;
use std::process::Command;

use parse::Parser as CParser;
use strings::StringTable;
use symbol::SymbolTable;
use target::Os;
use target::{classify_std, StdRequest, Target};
use token::{
    preprocess_asm_file, preprocess_with_defines, replace_trigraphs, show_token, token_type_name,
    AsmPreprocessConfig, PreprocessConfig, StreamTable, Tokenizer,
};

// ============================================================================
// Runtime Library Selection
// ============================================================================

/// Runtime library for soft-float and complex operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeLib {
    /// GNU runtime library (libgcc) - default on Linux/FreeBSD
    Libgcc,
    /// LLVM compiler-rt - default on macOS
    CompilerRt,
}

impl RuntimeLib {
    /// Get the default runtime library for a target
    pub fn default_for_target(target: &Target) -> Self {
        match target.os {
            Os::MacOS => RuntimeLib::CompilerRt,
            Os::Linux | Os::FreeBSD => RuntimeLib::Libgcc,
        }
    }
}

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("c17 - compile standard C programs"))]
struct Args {
    #[arg(required_unless_present = "print_targets", help = gettext("Input files"))]
    files: Vec<String>,

    /// Print registered targets
    #[arg(long = "print-targets", help = gettext("Display available target architectures"))]
    print_targets: bool,

    /// Dump tokens (for debugging tokenizer)
    #[arg(long = "dump-tokens", help = gettext("Dump tokens to stdout"))]
    dump_tokens: bool,

    /// Run preprocessor and dump result
    #[arg(short = 'E', help = gettext("Preprocess only, output to stdout"))]
    preprocess_only: bool,

    /// Dump AST (for debugging parser)
    #[arg(long = "dump-ast", help = gettext("Parse and dump AST to stdout"))]
    dump_ast: bool,

    /// Dump IR at a named stage (for debugging)
    /// Stages: post-linearize, post-mapping, post-opt, post-lower, all
    /// Bare --dump-ir = post-opt (backward compat)
    #[arg(long = "dump-ir", value_name = "stage", default_missing_value = "post-opt",
          num_args = 0..=1, help = gettext("Dump IR at stage (post-linearize, post-mapping, post-opt, post-lower, all)"))]
    dump_ir: Option<String>,

    /// Filter IR dumps to a specific function name
    #[arg(long = "dump-ir-func", value_name = "name", help = gettext("Only dump IR for this function"))]
    dump_ir_func: Option<String>,

    /// Verbose output (include position info)
    #[arg(
        short = 'v',
        long = "verbose",
        help = gettext("Verbose output with position info")
    )]
    verbose: bool,

    #[arg(short = 'D', action = clap::ArgAction::Append, value_name = "macro", help = gettext("Define a macro (-D name or -D name=value)"))]
    defines: Vec<String>,

    #[arg(short = 'U', action = clap::ArgAction::Append, value_name = "macro", help = gettext("Undefine a macro"))]
    undefines: Vec<String>,

    #[arg(short = 'I', action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add include path"))]
    include_paths: Vec<String>,

    /// Disable all standard include paths (system and builtin)
    #[arg(long = "nostdinc", help = gettext("Disable standard system include paths"))]
    no_std_inc: bool,

    /// Disable builtin include paths only (keep system paths)
    #[arg(long = "nobuiltininc", help = gettext("Disable builtin #include directories"))]
    no_builtin_inc: bool,

    /// Compile and assemble, but do not link
    #[arg(short = 'c', help = gettext("Compile and assemble, but do not link"))]
    compile_only: bool,

    /// Compile only; do not assemble or link (output assembly)
    #[arg(short = 'S', help = gettext("Compile only; output assembly"))]
    asm_only: bool,

    /// Place output in file
    #[arg(short = 'o', value_name = "file", help = gettext("Place output in file"))]
    output: Option<String>,

    /// Generate debug information (DWARF)
    /// Allow multiple -g flags (common in build systems)
    #[arg(short = 'g', action = clap::ArgAction::Count, help = gettext("Generate debug information"))]
    debug: u8,

    /// Disable CFI unwind tables (enabled by default)
    #[arg(long = "fno-unwind-tables", help = gettext("Disable CFI unwind table generation"))]
    no_unwind_tables: bool,

    /// Generate position-independent code (for shared libraries)
    /// Set by preprocess_args() when -fPIC or -fpic is passed
    #[arg(long = "c17-fpic", hide = true)]
    fpic: bool,

    /// Generate position-independent executable code (PIE)
    /// Set by preprocess_args() when -fPIE or -fpie is passed
    #[arg(long = "c17-fpie", action = clap::ArgAction::SetTrue, hide = true)]
    fpie: bool,

    /// Disable PIE code generation (GCC compatibility)
    /// Set by preprocess_args() when -fno-pie is passed
    #[arg(long = "c17-fno-pie", action = clap::ArgAction::SetTrue, hide = true)]
    fno_pie: bool,

    /// Produce a shared library
    #[arg(long = "shared", help = gettext("Produce a shared library"))]
    shared: bool,

    /// Target triple (e.g., aarch64-apple-darwin, x86_64-unknown-linux-gnu)
    #[arg(long = "target", value_name = "triple", help = gettext("Target triple for cross-compilation"))]
    target: Option<String>,

    /// Runtime library to use (libgcc or compiler-rt)
    /// Default: libgcc on Linux/FreeBSD, compiler-rt on macOS
    #[arg(long = "rtlib", value_name = "library", help = gettext("Runtime library (libgcc, compiler-rt)"))]
    rtlib: Option<String>,

    /// Optimization level (0=none, 1+=basic optimizations)
    /// -O alone means -O1, -O0 means none, -O2/-O3 mapped to -O1
    #[arg(short = 'O', default_value = "0", default_missing_value = "1",
          num_args = 0..=1, value_name = "level", help = gettext("Optimization level"))]
    opt_level: u32,

    #[arg(short = 'W', action = clap::ArgAction::Append, value_name = "warning",
          num_args = 0..=1, default_missing_value = "extra", help = gettext("Warning flags (e.g., -Wall, -Wextra, -Wno-unused)"))]
    warnings: Vec<String>,

    #[arg(short = 'w', help = gettext("Suppress all warnings"))]
    no_warnings: bool,

    #[arg(long = "pedantic", hide = true, help = gettext("Pedantic mode (compatibility)"))]
    pedantic: bool,

    /// C standard dialect, from `-std=` (rewritten by `preprocess_args_from`).
    ///
    /// Hidden because the user-facing spelling is `-std=`, which clap cannot
    /// express directly.
    #[arg(long = "c17-std", hide = true, value_name = "std")]
    c17_std: Option<String>,

    /// Print compilation statistics (for capacity tuning)
    #[arg(long = "stats", help = gettext("Print compilation statistics"))]
    stats: bool,

    #[arg(short = 'L', action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add library search path (passed to linker)"))]
    lib_paths: Vec<String>,

    #[arg(short = 'l', action = clap::ArgAction::Append, value_name = "library", help = gettext("Link library (passed to linker)"))]
    libraries: Vec<String>,

    /// Runtime (dynamic linker) search path — POSIX -R.
    #[arg(short = 'R', action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add directory to the runtime library search path"))]
    run_paths: Vec<String>,

    /// Library binding preference — POSIX -B dynamic|static.
    #[arg(short = 'B', value_name = "mode", help = gettext("Library binding: 'dynamic' or 'static'"))]
    binding: Option<String>,

    /// Produce a shared object — POSIX -G. Equivalent to --shared.
    #[arg(short = 'G', help = gettext("Produce a shared object"))]
    shared_object: bool,

    /// Strip symbol and line information from the output — POSIX -s.
    #[arg(short = 's', help = gettext("Strip symbol table and relocation information"))]
    strip: bool,

    /// Enable translation phase 1 trigraph replacement.
    ///
    /// Off by default: the replacement applies everywhere, including inside
    /// string literals, so `"What??!"` would silently become `"What|"`.
    #[arg(long = "trigraphs", help = gettext("Enable trigraph replacement (C17 5.2.1.1)"))]
    trigraphs: bool,

    /// Disable builtin function recognition (GCC compatibility)
    /// c17 does not implicitly recognize standard library functions as builtins,
    /// so this flag is accepted for compatibility but has no effect.
    #[arg(long = "fno-builtin", help = gettext("Disable builtin function recognition"))]
    fno_builtin: bool,

    /// Disable specific builtin function (GCC compatibility)
    /// Accepts -fno-builtin-FUNC format via preprocess_args
    #[arg(long = "c17-fno-builtin-func", action = clap::ArgAction::Append, value_name = "func", hide = true)]
    fno_builtin_funcs: Vec<String>,

    /// Extra flags to pass through to the linker (set by preprocess_args)
    #[arg(long = "c17-linker-flag", action = clap::ArgAction::Append, value_name = "flag", hide = true)]
    linker_flags: Vec<String>,

    /// Unsupported machine flags captured by preprocess_args
    #[arg(long = "c17-unsupported-mflag", action = clap::ArgAction::Append, value_name = "flag", hide = true)]
    unsupported_mflags: Vec<String>,
}

/// The `-Wno-` name for the "`-std=` was not honoured" warning.
const STD_DIALECT_WARNING: &str = "c17-dialect";

impl Args {
    /// Classify `-std=`, if one was given.
    ///
    /// c17 compiles one language, so this cannot select anything -- it only
    /// separates a spelling we recognize from a typo. Returns the offending
    /// spelling on failure so the caller can name it in the diagnostic.
    fn std_request(&self) -> Result<Option<StdRequest>, &str> {
        match &self.c17_std {
            None => Ok(None),
            Some(spec) => classify_std(spec).map(Some).ok_or(spec.as_str()),
        }
    }

    /// Is the warning named `name` turned off, by `-w` or `-Wno-<name>`?
    fn warning_suppressed(&self, name: &str) -> bool {
        self.no_warnings
            || self
                .warnings
                .iter()
                .any(|w| w.strip_prefix("no-") == Some(name))
    }
}

/// Valid stage names for --dump-ir.
const DUMP_IR_STAGES: &[&str] = &[
    "post-linearize",
    "post-mapping",
    "post-opt",
    "post-lower",
    "all",
];

/// Validate --dump-ir stage name. Returns error message if invalid.
fn validate_dump_ir_stage(stage: &str) -> Result<(), String> {
    if DUMP_IR_STAGES.contains(&stage) {
        Ok(())
    } else {
        Err(format!(
            "unknown --dump-ir stage '{}'. Valid stages: {}",
            stage,
            DUMP_IR_STAGES.join(", ")
        ))
    }
}

/// Check if IR should be dumped at the given stage.
fn should_dump_ir(args: &Args, stage: &str) -> bool {
    match args.dump_ir.as_deref() {
        Some("all") => true,
        Some(s) => s == stage,
        None => false,
    }
}

/// Dump IR at a named pipeline stage.
fn dump_ir(args: &Args, module: &ir::Module, stage: &str) {
    if !should_dump_ir(args, stage) {
        return;
    }
    eprintln!("=== {} ===", stage);
    match &args.dump_ir_func {
        Some(name) => {
            for func in &module.functions {
                if func.name == *name {
                    print!("{}", func);
                }
            }
        }
        None => print!("{}", module),
    }
}

/// Print compilation statistics for capacity tuning
fn print_stats(
    path: &str,
    strings: &StringTable,
    types: &types::TypeTable,
    symbols: &SymbolTable,
    module: &ir::Module,
) {
    // Calculate function statistics
    let num_functions = module.functions.len();
    let (max_pseudos, max_blocks, max_locals, max_insns) =
        module
            .functions
            .iter()
            .fold((0, 0, 0, 0), |(max_p, max_b, max_l, max_i), func| {
                let max_insns_in_func =
                    func.blocks.iter().map(|b| b.insns.len()).max().unwrap_or(0);
                (
                    max_p.max(func.pseudos.len()),
                    max_b.max(func.blocks.len()),
                    max_l.max(func.locals.len()),
                    max_i.max(max_insns_in_func),
                )
            });

    eprintln!("=== Compilation Statistics: {} ===", path);
    eprintln!("StringTable: {} strings", strings.len());
    eprintln!("TypeTable: {} types", types.len());
    eprintln!("SymbolTable: {} symbols", symbols.len());
    eprintln!("String literals: {}", module.strings.len());
    eprintln!("Globals: {}", module.globals.len());
    eprintln!(
        "Functions: {} (max_pseudos={}, max_blocks={}, max_locals={})",
        num_functions, max_pseudos, max_blocks, max_locals
    );
    eprintln!("Max instructions/block: {}", max_insns);
    eprintln!();
}

/// Where a compiled source operand's object file goes.
enum ObjectName {
    /// `-c`: a named object file the user keeps.
    Keep(String),
    /// Link mode: a temporary this process owns and removes after linking.
    Temp(String),
}

/// What compiling one source operand produced.
enum Compiled {
    /// No object: an early-exit mode (`-E`, `-S`, `--dump-*`) ran instead.
    Nothing,
    /// An object file, plus whether it is a temporary this process must remove.
    Object { path: String, temporary: bool },
}

fn process_file(
    path: &str,
    streams: &mut StreamTable,
    args: &Args,
    target: &Target,
    obj_name: &ObjectName,
    scratch: &Path,
    operand_id: usize,
) -> io::Result<Compiled> {
    // Read file (or stdin if path is "-")
    let mut buffer = Vec::new();
    let display_path = if path == "-" {
        io::stdin().read_to_end(&mut buffer)?;
        "<stdin>"
    } else {
        let file = File::open(path)?;
        let mut reader = BufReader::new(file);
        reader.read_to_end(&mut buffer)?;
        path
    };

    // Translation phase 1, before anything else looks at the bytes.
    let buffer = if args.trigraphs {
        replace_trigraphs(&buffer).into_owned()
    } else {
        buffer
    };

    // Create stream
    let stream_id = streams.add(display_path.to_string());

    // Create shared string table for identifier interning
    let mut strings = StringTable::new();

    // Tokenize
    let tokens = {
        let mut tokenizer = Tokenizer::new(&buffer, stream_id, &mut strings);
        tokenizer.tokenize()
    };

    // Dump raw tokens if requested
    if args.dump_tokens && !args.preprocess_only {
        for token in &tokens {
            if args.verbose {
                println!(
                    "{:>4}:{:<3} {:12} {}",
                    token.pos.line,
                    token.pos.col,
                    token_type_name(token.typ),
                    show_token(token, &strings)
                );
            } else {
                let text = show_token(token, &strings);
                // Skip stream markers (e.g., <STREAM_BEGIN>, <STREAM_END>)
                // but NOT the '<' operator or '<=', etc.
                if !(text.starts_with("<STREAM")
                    || text.starts_with("<ident")
                    || text.starts_with("<special"))
                {
                    print!("{} ", text);
                }
            }
        }
        if !args.verbose {
            println!();
        }
        return Ok(Compiled::Nothing);
    }

    // Preprocess (may add new identifiers from included files)
    let preprocessed = preprocess_with_defines(
        tokens,
        target,
        &mut strings,
        path,
        &PreprocessConfig {
            defines: &args.defines,
            undefines: &args.undefines,
            include_paths: &args.include_paths,
            no_std_inc: args.no_std_inc,
            no_builtin_inc: args.no_builtin_inc,
            trigraphs: args.trigraphs,
        },
    );

    if args.preprocess_only {
        // Output preprocessed tokens.
        //
        // STDOUT (88032-88038) requires the -E output to carry at least one
        // `# <line> "<file>"` line for each file processed via #include, so
        // that a consumer can attribute the text; RATIONALE (88370-88374)
        // names makefile dependency generation as the purpose. The markers
        // were being discarded outright.
        //
        // `include_file` strips the included stream's begin/end tokens, so the
        // transition is detected from `pos.stream` instead. The trailing flag
        // follows GCC: 1 on entering a file, 2 on returning to one.
        // Start by naming the primary source, as GCC does: a consumer needs
        // that even when the first token comes from an #include.
        //
        // The line numbers are physical. `#line` is *not* reflected: it sets
        // state on the preprocessor and is never recorded in the stream
        // registry, so `effective_position` cannot see it either — the same
        // pre-existing gap that keeps parser diagnostics on physical lines.
        println!("# 1 \"{}\"", display_path);
        let mut emitted_marker_for: Vec<u16> = vec![stream_id];
        let mut current_stream: Option<u16> = Some(stream_id);
        let mut at_line_start = true;

        let mut iter = preprocessed.iter().peekable();
        while let Some(token) = iter.next() {
            if args.verbose {
                println!(
                    "{:>4}:{:<3} {:12} {}",
                    token.pos.line,
                    token.pos.col,
                    token_type_name(token.typ),
                    show_token(token, &strings)
                );
            } else {
                let text = show_token(token, &strings);
                // Skip stream markers (e.g., <STREAM_BEGIN>, <STREAM_END>)
                // but NOT the '<' operator or '<=', etc.
                if text.starts_with("<STREAM")
                    || text.starts_with("<ident")
                    || text.starts_with("<special")
                {
                    continue;
                }

                if current_stream != Some(token.pos.stream) {
                    let (name, line, _) = diag::effective_position(token.pos);
                    let returning = emitted_marker_for.contains(&token.pos.stream);
                    if !returning {
                        emitted_marker_for.push(token.pos.stream);
                    }
                    if !at_line_start {
                        println!();
                    }
                    println!("# {} \"{}\" {}", line, name, if returning { 2 } else { 1 });
                    current_stream = Some(token.pos.stream);
                }

                print!("{}", text);
                at_line_start = false;
                // Check next token to determine separator
                if let Some(next) = iter.peek() {
                    if next.pos.newline {
                        println!();
                        at_line_start = true;
                    } else {
                        // Need a space if:
                        // 1. Original had whitespace, OR
                        // 2. Adjacent tokens would merge (both alphanumeric/underscore)
                        let next_text = show_token(next, &strings);
                        let needs_space = next.pos.whitespace
                            || (text
                                .chars()
                                .last()
                                .is_some_and(|c| c.is_alphanumeric() || c == '_')
                                && next_text
                                    .chars()
                                    .next()
                                    .is_some_and(|c| c.is_alphanumeric() || c == '_'));
                        if needs_space {
                            print!(" ");
                        }
                    }
                }
            }
        }
        if !args.verbose {
            println!();
        }
        // Check for preprocessor errors (e.g., #error directive)
        if diag::has_error() != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "preprocessing failed",
            ));
        }
        return Ok(Compiled::Nothing);
    }

    // Create symbol table and type table BEFORE parsing
    // symbols are bound during parsing
    let mut symbols = SymbolTable::new();
    let mut types = types::TypeTable::new(target);

    // Parse (this also binds symbols to the symbol table)
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = parser
        .parse_translation_unit()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {}", e)))?;

    // Check for semantic errors (e.g., undeclared identifiers) reported during parsing
    if diag::has_error() != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "compilation failed",
        ));
    }

    if let Some(stage) = &args.dump_ir {
        if let Err(msg) = validate_dump_ir_stage(stage) {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, msg));
        }
    }

    if args.dump_ast {
        println!("{:#?}", ast);
        return Ok(Compiled::Nothing);
    }

    // Linearize to IR
    let mut module =
        ir::linearize::linearize(&ast, &symbols, &types, &strings, target, args.debug > 0);

    // Check for errors during linearization (e.g., unsupported global initializers)
    if diag::has_error() != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "compilation failed",
        ));
    }

    // Print compilation statistics if requested
    if args.stats {
        print_stats(path, &strings, &types, &symbols, &module);
    }

    // Set DWARF metadata
    module.source_name = Some(path.to_string());
    module.comp_dir = std::env::current_dir()
        .ok()
        .map(|p| p.to_string_lossy().to_string());

    dump_ir(args, &module, "post-linearize");

    // Hardware mapping pass — centralized target-specific lowering decisions
    arch::mapping::run_mapping(&mut module, &types, target);

    dump_ir(args, &module, "post-mapping");

    // Optimize IR (if enabled)
    if args.opt_level > 0 {
        opt::optimize_module(&mut module, args.opt_level);
    }

    dump_ir(args, &module, "post-opt");

    if args.dump_ir.is_some() && !should_dump_ir(args, "post-lower") {
        return Ok(Compiled::Nothing);
    }

    // Lower IR (phi elimination, etc.)
    ir::lower::lower_module(&mut module);

    dump_ir(args, &module, "post-lower");

    if args.dump_ir.is_some() {
        return Ok(Compiled::Nothing);
    }

    // Generate assembly
    let emit_unwind_tables = !args.no_unwind_tables;
    let pie_mode = pie_enabled(args, target);
    let pic_mode = args.fpic || producing_shared(args) || pie_mode;
    let shared_mode = producing_shared(args);
    let mut codegen =
        arch::codegen::create_codegen(target.clone(), emit_unwind_tables, pic_mode, shared_mode);
    let asm = codegen.generate(&module, &types);

    // Determine output file names
    // For stdin ("-"), use "stdin" as the default stem
    let stem = if path == "-" {
        "stdin"
    } else {
        let input_path = Path::new(path);
        input_path
            .file_stem()
            .unwrap_or_default()
            .to_str()
            .unwrap_or("a")
    };

    if args.asm_only {
        // Output assembly
        let asm_file = args.output.clone().unwrap_or_else(|| format!("{}.s", stem));
        if asm_file == "-" {
            // Write to stdout
            print!("{}", asm);
        } else {
            let mut file = File::create(&asm_file)?;
            file.write_all(asm.as_bytes())?;
            if args.verbose {
                eprintln!("Wrote assembly to {}", asm_file);
            }
        }
        return Ok(Compiled::Nothing);
    }

    // Write the assembly to a scratch file for the assembler. It lives in the
    // per-run scratch directory, so the name only has to be unique within this
    // process — several operands are compiled in one run now.
    let temp_asm = scratch_path(scratch, operand_id, stem, "s");
    {
        let mut file = File::create(&temp_asm)?;
        file.write_all(asm.as_bytes())?;
    }

    // Assemble. The caller decided where the object goes; this function no
    // longer links, so that one link can cover every operand — POSIX EXAMPLE 1
    // and EXAMPLE 3 both combine sources with objects and libraries.
    let (obj_file, temporary) = match obj_name {
        ObjectName::Keep(p) => (p.clone(), false),
        ObjectName::Temp(p) => (p.clone(), true),
    };

    let mut as_cmd = Command::new("as");
    if args.debug > 0 {
        as_cmd.arg("-g");
    }
    let status = as_cmd.args(["-o", &obj_file, &temp_asm]).status()?;

    let _ = std::fs::remove_file(&temp_asm);

    if !status.success() {
        return Err(io::Error::other("assembler failed"));
    }

    if args.verbose && !temporary {
        eprintln!("Wrote object file to {}", obj_file);
    }

    Ok(Compiled::Object {
        path: obj_file,
        temporary,
    })
}

/// One entry on the resolved link line.
///
/// This is `linkargs::LinkArg` after each source operand has been replaced by
/// the object it compiled to, and each non-link operand dropped.
enum LinkItem {
    Object(String),
    LibPath(String),
    Library(String),
    RunPath(String),
}

/// Link `link_line` into `exe_file`, preserving the order given.
fn link_objects(
    link_line: &[LinkItem],
    exe_file: &str,
    args: &Args,
    target: &Target,
) -> io::Result<()> {
    let mut link_cmd = Command::new("cc");
    if producing_shared(args) {
        link_cmd.arg("-shared");
    } else if pie_enabled(args, target) {
        link_cmd.arg("-pie");
    } else {
        link_cmd.arg("-no-pie");
    }
    link_cmd.args(["-o", exe_file]);

    // -B selects which form of a library `-l` prefers. GNU ld spells this
    // -Bstatic / -Bdynamic and it is positional, so it goes ahead of the
    // libraries it governs.
    //
    // Apple's linker has neither spelling and rejects both outright — it has
    // no general way to say "prefer the archive", only naming the .a directly.
    // Dynamic is what it does anyway, so honoring `-B dynamic` there means
    // emitting nothing; `-B static` cannot be honored, and saying so is better
    // than a link that silently ignores it.
    let gnu_binding = target.os != Os::MacOS;
    match args.binding.as_deref() {
        Some("static") if gnu_binding => {
            link_cmd.arg("-Wl,-Bstatic");
        }
        Some("static") => {
            eprintln!(
                "{}",
                gettext("c17: warning: -B static: this platform's linker cannot prefer archives")
            );
        }
        Some("dynamic") if gnu_binding => {
            link_cmd.arg("-Wl,-Bdynamic");
        }
        _ => {}
    }

    // Emit the recovered link line in argument order: a library is searched
    // where its name was encountered, not after every object.
    for item in link_line {
        match item {
            LinkItem::Object(p) => {
                link_cmd.arg(p);
            }
            LinkItem::LibPath(d) => {
                link_cmd.arg(format!("-L{}", d));
            }
            LinkItem::Library(l) => {
                link_cmd.arg(format!("-l{}", l));
            }
            LinkItem::RunPath(d) => {
                link_cmd.arg(format!("-Wl,-rpath,{}", d));
            }
        }
    }

    // -B governs only the libraries named on the command line. The host driver
    // appends its own (libc, libgcc_s) after everything here, and those are not
    // usually available as archives, so binding is restored before them.
    if args.binding.as_deref() == Some("static") && gnu_binding {
        link_cmd.arg("-Wl,-Bdynamic");
    }

    if args.strip {
        link_cmd.arg("-s");
    }

    for flag in &args.linker_flags {
        link_cmd.arg(flag);
    }

    if !link_cmd.status()?.success() {
        return Err(io::Error::other("linker failed"));
    }

    if args.verbose {
        eprintln!("Linked to {}", exe_file);
    }
    Ok(())
}

/// Check if a string is a valid optimization level for -O
fn is_valid_opt_level(s: &str) -> bool {
    matches!(s, "0" | "1" | "2" | "3" | "s" | "z" | "fast" | "g")
}

/// Preprocess this process's command-line arguments for gcc compatibility.
fn preprocess_args() -> Vec<String> {
    preprocess_args_from(std::env::args().collect())
}

/// Preprocess command-line arguments for gcc compatibility.
/// - Converts -Wall → -W all, -Wextra → -W extra, etc.
/// - Handles -O flag: standalone -O followed by non-level becomes -O1
///
/// Takes the raw argument vector rather than reading the environment so the
/// unit tests exercise this exact function. They previously re-implemented it,
/// and the copy had already drifted.
fn preprocess_args_from(raw_args: Vec<String>) -> Vec<String> {
    let mut result = Vec::with_capacity(raw_args.len());
    let mut i = 0;
    let mut o_flag_idx: Option<usize> = None; // index into result of the -O flag
    let mut std_flag_idx: Option<usize> = None; // index into result of the -std= value
    let mut seen_fpic = false;

    while i < raw_args.len() {
        let arg = &raw_args[i];

        if arg == "-O" {
            // Standalone -O: check if next arg is a valid optimization level
            let new_flag = if i + 1 < raw_args.len() && is_valid_opt_level(&raw_args[i + 1]) {
                let flag = format!("-O{}", raw_args[i + 1]);
                i += 2;
                flag
            } else {
                i += 1;
                "-O1".to_string()
            };
            // Last -O flag wins (GCC convention)
            if let Some(idx) = o_flag_idx {
                result[idx] = new_flag;
            } else {
                o_flag_idx = Some(result.len());
                result.push(new_flag);
            }
        } else if arg.starts_with("-O") && arg.len() > 2 {
            // -O0, -O1, -O2, -O3, -Os, -Oz, -Ofast, -Og — last one wins
            if let Some(idx) = o_flag_idx {
                result[idx] = arg.clone();
            } else {
                o_flag_idx = Some(result.len());
                result.push(arg.clone());
            }
            i += 1;
        } else if arg.starts_with("-W") && arg.len() > 2 && !arg.starts_with("-Wl,") {
            // -Wall → -W all, -Wextra → -W extra, etc.
            result.push("-W".to_string());
            result.push(arg[2..].to_string());
            i += 1;
        } else if arg.starts_with("-L") && arg.len() > 2 {
            // -L. → -L .
            result.push("-L".to_string());
            result.push(arg[2..].to_string());
            i += 1;
        } else if arg.starts_with("-l") && arg.len() > 2 {
            // -lz → -l z
            result.push("-l".to_string());
            result.push(arg[2..].to_string());
            i += 1;
        } else if arg.starts_with("-R") && arg.len() > 2 {
            // -R/opt/lib → -R /opt/lib
            result.push("-R".to_string());
            result.push(arg[2..].to_string());
            i += 1;
        } else if arg.starts_with("-B") && arg.len() > 2 {
            // -Bstatic → -B static
            result.push("-B".to_string());
            result.push(arg[2..].to_string());
            i += 1;
        } else if let Some(spec) = arg.strip_prefix("-std=") {
            // -std=c17 → --c17-std c17 (internal flag), so clap can see it.
            //
            // Last one wins, as in gcc. Passing each occurrence through would
            // make a second -std= a fatal "cannot be used multiple times",
            // and build systems routinely accumulate one from configure and
            // another from a makefile. -O just above does the same thing.
            if let Some(idx) = std_flag_idx {
                result[idx] = spec.to_string();
            } else {
                result.push("--c17-std".to_string());
                std_flag_idx = Some(result.len());
                result.push(spec.to_string());
            }
            i += 1;
        } else if arg == "-fPIC" || arg == "-fpic" {
            // -fPIC / -fpic → --c17-fpic (internal flag) (first one only)
            if !seen_fpic {
                result.push("--c17-fpic".to_string());
                seen_fpic = true;
            }
            i += 1;
        } else if arg == "-fPIE" || arg == "-fpie" {
            // -fPIE / -fpie → --c17-fpie (internal flag)
            result.push("--c17-fpie".to_string());
            i += 1;
        } else if arg == "-fno-pie" {
            // -fno-pie → --c17-fno-pie (internal flag)
            result.push("--c17-fno-pie".to_string());
            i += 1;
        } else if arg == "-shared" {
            // -shared → --shared
            result.push("--shared".to_string());
            i += 1;
        } else if arg == "-fno-builtin" {
            // -fno-builtin → --fno-builtin
            result.push("--fno-builtin".to_string());
            i += 1;
        } else if let Some(func) = arg.strip_prefix("-fno-builtin-") {
            // -fno-builtin-FUNC → --c17-fno-builtin-func FUNC
            result.push("--c17-fno-builtin-func".to_string());
            result.push(func.to_string());
            i += 1;
        } else if arg == "-fstrict-overflow"
            || arg == "-fno-strict-overflow"
            || arg == "-fwrapv"
            || arg == "-fstrict-aliasing"
            || arg == "-fno-strict-aliasing"
        {
            // GCC optimization flags - silently ignore (c17 doesn't have these optimizations)
            i += 1;
        } else if arg.starts_with("-m") && arg.len() > 2 {
            // Machine flags - unsupported (SIMD/arch not supported yet)
            result.push(format!("--c17-unsupported-mflag={}", arg));
            i += 1;
        } else if arg.starts_with("-fvisibility")
            || arg == "-fno-semantic-interposition"
            || arg.starts_with("-fstack-protector")
            || arg == "-fno-reorder-blocks-and-partition"
            || arg == "-fno-plt"
            || arg == "-fno-common"
            || arg == "-fexceptions"
            || arg == "-fno-exceptions"
        {
            // GCC flags - silently ignore
            i += 1;
        } else if arg.starts_with("-fsanitize") {
            // Sanitizer flags - silently ignore (c17 doesn't support sanitizers)
            i += 1;
        } else if arg == "-ffreestanding" || arg == "-fhosted" {
            // Not swallowed by the catch-all below: there is no freestanding
            // mode to enter (see #H1 — we do not bundle the freestanding
            // header set), so accepting the flag and ignoring it would be a
            // lie. Diagnose instead. `-fhosted` is what we already are.
            if arg == "-ffreestanding" {
                eprintln!(
                    "c17: {}",
                    gettext(
                        "-ffreestanding is not supported: no freestanding environment is provided"
                    )
                );
                std::process::exit(1);
            }
            i += 1;
        } else if arg.starts_with("-f") && !arg.starts_with("-fno-builtin") {
            // Catch-all: silently ignore any other -f* flag we don't handle
            i += 1;
        } else if arg == "-p" || arg == "-pg" {
            // Profiling flags - silently ignore (c17 doesn't support profiling)
            i += 1;
        } else if arg == "-pipe" {
            // Misc GCC flags - silently ignore
            i += 1;
        } else if arg == "-pie" {
            // -pie enables PIE link mode
            result.push("--c17-fpie".to_string());
            result.push("--c17-linker-flag=-pie".to_string());
            i += 1;
        } else if arg == "-no-pie" {
            // -no-pie disables PIE link mode
            result.push("--c17-fno-pie".to_string());
            result.push("--c17-linker-flag=-no-pie".to_string());
            i += 1;
        } else if let Some(wl_args) = arg.strip_prefix("-Wl,") {
            // -Wl,flag1,flag2 -> pass each flag to linker
            for flag in wl_args.split(',') {
                result.push(format!("--c17-linker-flag={}", flag));
            }
            i += 1;
        } else if arg == "-Xlinker" {
            // -Xlinker <arg> -> pass next arg to linker
            if i + 1 < raw_args.len() {
                result.push(format!("--c17-linker-flag={}", raw_args[i + 1]));
                i += 2;
            } else {
                i += 1;
            }
        } else if arg == "-pthread" {
            // -pthread -> pass to linker and define _REENTRANT
            result.push("--c17-linker-flag=-pthread".to_string());
            result.push("-D".to_string());
            result.push("_REENTRANT".to_string());
            i += 1;
        } else if arg == "-rdynamic" {
            // -rdynamic -> pass to linker
            result.push("--c17-linker-flag=-rdynamic".to_string());
            i += 1;
        } else if arg == "--print-multiarch" {
            // GCC compatibility: print multiarch tuple and exit
            let target = Target::host();
            match (target.arch, target.os) {
                (target::Arch::X86_64, Os::Linux) => println!("x86_64-linux-gnu"),
                (target::Arch::Aarch64, Os::Linux) => println!("aarch64-linux-gnu"),
                _ => {} // Empty output for unsupported platforms
            }
            std::process::exit(0);
        } else if let Some(prog) = arg.strip_prefix("-print-prog-name=") {
            // GCC compatibility: print program path and exit
            // Just echo back the program name (like gcc does when it doesn't have a special path)
            println!("{}", prog);
            std::process::exit(0);
        } else if arg == "-v" || arg == "--version" || arg == "-qversion" || arg == "-version" {
            // Version query - handled by clap, but -v is also our verbose flag
            // Let it pass through to clap
            result.push(arg.clone());
            i += 1;
        } else {
            result.push(arg.clone());
            i += 1;
        }
    }

    result
}

/// Check if a file is a C source file (by extension)
fn is_source_file(path: &str) -> bool {
    path.ends_with(".c") || path.ends_with(".i") || path == "-"
}

/// Check if a file is an assembly file (by extension)
/// .s = pure assembly, .S = assembly with C preprocessor directives
fn is_asm_file(path: &str) -> bool {
    path.ends_with(".s") || path.ends_with(".S")
}

/// Check if a file is an object file or library (by extension)
fn is_object_file(path: &str) -> bool {
    path.ends_with(".o")
        || path.ends_with(".a")
        || path.ends_with(".so")
        || path.ends_with(".dylib")
        || path.contains(".so.") // versioned .so files like libz.so.1.3.1
}

/// What kind of thing a pathname operand names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperandKind {
    /// `.c`, `.i`, or a bare `-`.
    Source,
    /// `.s` or `.S`.
    Asm,
    /// `.o`, `.a`, `.so`, `.dylib`, or a versioned `.so.N`.
    Object,
    /// Anything else: warned about and skipped.
    Unknown,
}

/// A pathname operand together with its kind, keeping argument order.
#[derive(Debug, Clone)]
struct Operand {
    path: String,
    kind: OperandKind,
}

impl Operand {
    fn classify(path: String) -> Self {
        let kind = if is_source_file(&path) {
            OperandKind::Source
        } else if is_asm_file(&path) {
            OperandKind::Asm
        } else if is_object_file(&path) {
            OperandKind::Object
        } else {
            OperandKind::Unknown
        };
        Operand { path, kind }
    }
}

/// Name a scratch file inside the per-run temporary directory.
///
/// The directory itself is created by `tempfile`, which is why these names
/// need no PID or randomness of their own: the directory is unpredictable,
/// created with `O_EXCL`, and removed when the run ends. That closes both the
/// old `/tmp/c17_<pid>.s` symlink hazard and the leak on every early exit.
///
/// The name is prefixed with the operand's position because one run now
/// compiles every operand into this one directory, and a file stem is not
/// unique across them: `c17 a/util.c b/util.c` gave both the same `util.o`,
/// so the second silently overwrote the first and the link named it twice.
fn scratch_path(scratch: &Path, operand_id: usize, stem: &str, ext: &str) -> String {
    scratch
        .join(format!("{}-{}.{}", operand_id, stem, ext))
        .to_string_lossy()
        .into_owned()
}

/// The file stem used to name a source operand's outputs.
fn operand_stem(path: &str) -> &str {
    if path == "-" {
        return "stdin";
    }
    Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("a")
}

/// Decide where a source operand's object file goes.
fn source_object_name(path: &str, args: &Args, scratch: &Path, operand_id: usize) -> ObjectName {
    let stem = operand_stem(path);
    if args.compile_only {
        // -c writes an object the user keeps. `-o` names it; otherwise it is
        // $(basename operand .c).o in the current directory.
        ObjectName::Keep(args.output.clone().unwrap_or_else(|| format!("{}.o", stem)))
    } else {
        ObjectName::Temp(scratch_path(scratch, operand_id, stem, "o"))
    }
}

/// Assemble a `.s`/`.S` operand.
///
/// Returns the object path, or `None` when `-c` wrote a named object that is
/// not a link input.
fn assemble_operand(
    path: &str,
    args: &Args,
    target: &Target,
    scratch: &Path,
    operand_id: usize,
) -> io::Result<Option<String>> {
    let stem = operand_stem(path);
    let obj_file = if args.compile_only {
        args.output.clone().unwrap_or_else(|| format!("{}.o", stem))
    } else {
        scratch_path(scratch, operand_id, stem, "o")
    };

    // .S files need preprocessing, .s files do not.
    let asm_to_assemble = if path.ends_with(".S") {
        let temp_s = scratch_path(scratch, operand_id, stem, "s");
        let content = std::fs::read(path)?;
        let asm_config = AsmPreprocessConfig {
            defines: &args.defines,
            undefines: &args.undefines,
            include_paths: &args.include_paths,
            no_std_inc: args.no_std_inc,
        };
        let preprocessed = preprocess_asm_file(&content, target, path, &asm_config);
        // Catch #error, a missing include, and friends.
        if diag::has_error() != 0 {
            diag::reset_counts();
            return Err(io::Error::other("preprocessing failed"));
        }
        std::fs::write(&temp_s, &preprocessed)?;
        temp_s
    } else {
        path.to_string()
    };

    let mut as_cmd = Command::new("as");
    if args.debug > 0 {
        as_cmd.arg("-g");
    }
    as_cmd.args(["-o", &obj_file, &asm_to_assemble]);
    let status = as_cmd.status()?;

    if path.ends_with(".S") {
        let _ = std::fs::remove_file(&asm_to_assemble);
    }

    if !status.success() {
        return Err(io::Error::other("assembler failed"));
    }

    if args.compile_only {
        Ok(None)
    } else {
        Ok(Some(obj_file))
    }
}

/// Build the final link line from the rescanned argument order.
///
/// `operand_objects` is indexed by pathname-operand position and holds the
/// object each operand contributes, if any. Walking `scanned` therefore places
/// every `-L`/`-l`/`-R` exactly where it appeared relative to the operands.
///
/// If the rescan disagrees with what clap collected — which would mean
/// `VALUE_OPTIONS` in `linkargs` has drifted from `Args` — the ordering is not
/// trustworthy, so this falls back to the unordered shape (every object, then
/// every `-L`, then every `-l`, then every `-R`) rather than emitting a
/// scrambled link line.
fn build_link_line(
    scanned: &[linkargs::LinkArg],
    args: &Args,
    operand_objects: &[Option<String>],
) -> Vec<LinkItem> {
    let scanned_operands: Vec<&String> = scanned
        .iter()
        .filter_map(|a| match a {
            linkargs::LinkArg::Operand(p) => Some(p),
            _ => None,
        })
        .collect();

    let ordering_recovered = scanned_operands.len() == args.files.len()
        && scanned_operands
            .iter()
            .zip(&args.files)
            .all(|(a, b)| *a == b);

    if !ordering_recovered {
        let mut out: Vec<LinkItem> = operand_objects
            .iter()
            .flatten()
            .cloned()
            .map(LinkItem::Object)
            .collect();
        out.extend(args.lib_paths.iter().cloned().map(LinkItem::LibPath));
        out.extend(args.libraries.iter().cloned().map(LinkItem::Library));
        out.extend(args.run_paths.iter().cloned().map(LinkItem::RunPath));
        return out;
    }

    let mut out = Vec::new();
    let mut operand_idx = 0;
    for item in scanned {
        match item {
            linkargs::LinkArg::Operand(_) => {
                if let Some(Some(obj)) = operand_objects.get(operand_idx) {
                    out.push(LinkItem::Object(obj.clone()));
                }
                operand_idx += 1;
            }
            linkargs::LinkArg::LibPath(d) => out.push(LinkItem::LibPath(d.clone())),
            linkargs::LinkArg::Library(l) => out.push(LinkItem::Library(l.clone())),
            linkargs::LinkArg::RunPath(d) => out.push(LinkItem::RunPath(d.clone())),
        }
    }
    out
}

/// Whether the output is a shared object.
///
/// POSIX spells this `-G`; `--shared` is the GCC-compatible long form that
/// predates it. They mean the same thing.
fn producing_shared(args: &Args) -> bool {
    args.shared || args.shared_object
}

/// Determine whether PIE should be enabled for this compilation.
fn pie_enabled(args: &Args, target: &Target) -> bool {
    if producing_shared(args) || args.fno_pie {
        return false;
    }
    if args.fpie {
        return true;
    }
    target.os == Os::Linux
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let argv = preprocess_args();
    // Rescan for the -L/-l/-R order relative to the operands, which clap's
    // per-flag collection cannot preserve. Done before parsing so a parse
    // failure still exits the usual way.
    let scanned = linkargs::scan(argv.iter().cloned());
    let args = Args::parse_from(argv);

    // Handle unsupported machine flags early
    if !args.unsupported_mflags.is_empty() {
        for flag in &args.unsupported_mflags {
            eprintln!("c17: {}: {}", gettext("unsupported machine flag"), flag);
        }
        std::process::exit(1);
    }

    if args.no_warnings {
        diag::suppress_warnings();
    }

    // Validate -std= alongside the other argument checks, before any
    // early-return path, so a typo is never silently accepted.
    match args.std_request() {
        Err(spec) => {
            eprintln!(
                "c17: {}: '{}'",
                gettext("unrecognized C standard for -std="),
                spec
            );
            std::process::exit(1);
        }
        // Say plainly that an older revision was not honoured. c17 compiles
        // C17 and only C17, so the flag is accepted -- build systems pass it
        // unconditionally -- but silently ignoring it is what let
        // __STDC_VERSION__ disagree with the binary's own name once already.
        Ok(Some(StdRequest::Older)) if !args.warning_suppressed(STD_DIALECT_WARNING) => {
            let spec = args.c17_std.as_deref().unwrap_or_default();
            eprintln!(
                "c17: {}: {}",
                gettext("warning"),
                gettext_args(
                    "'-std={0}' ignored; c17 compiles C17 (ISO/IEC 9899:2018) only",
                    &[spec]
                )
            );
        }
        Ok(_) => {}
    }

    // Handle --print-targets
    if args.print_targets {
        println!("  Registered Targets:");
        println!("    aarch64    - AArch64 (little endian)");
        println!("    x86-64     - 64-bit X86: EM64T and AMD64");
        return Ok(());
    }

    // Detect target (use --target if specified, otherwise detect host)
    let target = if let Some(ref triple) = args.target {
        match Target::from_triple(triple) {
            Some(t) => t,
            None => {
                eprintln!("c17: {}: {}", gettext("unsupported target"), triple);
                std::process::exit(1);
            }
        }
    } else {
        Target::host()
    };

    // Parse runtime library selection
    let _rtlib = match args.rtlib.as_deref() {
        Some("libgcc") => RuntimeLib::Libgcc,
        Some("compiler-rt") => RuntimeLib::CompilerRt,
        Some(other) => {
            eprintln!(
                "c17: {}: '{}' ({})",
                gettext("unknown runtime library"),
                other,
                gettext("use 'libgcc' or 'compiler-rt'")
            );
            std::process::exit(1);
        }
        None => RuntimeLib::default_for_target(&target),
    };

    // Classify operands in a single pass so that argument order survives.
    // The spec deviates from XBD 12.2 to make that order significant
    // (87866-87867), and EXAMPLE 3 depends on it.
    let operands: Vec<Operand> = args
        .files
        .iter()
        .map(|f| Operand::classify(f.clone()))
        .collect();

    for op in &operands {
        if op.kind == OperandKind::Unknown {
            eprintln!(
                "c17: {}: {}: {}",
                gettext("warning"),
                gettext("unrecognized file type"),
                op.path
            );
        }
    }

    let source_count = operands
        .iter()
        .filter(|o| o.kind == OperandKind::Source)
        .count();

    // A single -o names one output. With -c and several sources it would name
    // each of them in turn, so every object but the last is overwritten. The
    // spec leaves this unspecified (88338-88343); say so rather than silently
    // producing one object.
    if args.compile_only && args.output.is_some() && source_count > 1 {
        eprintln!(
            "c17: {}: {} ({})",
            gettext("warning"),
            gettext("-o applies only to the last source operand with -c"),
            source_count
        );
    }

    if let Some(mode) = args.binding.as_deref() {
        if mode != "dynamic" && mode != "static" {
            eprintln!(
                "c17: -B: {}: '{}'",
                gettext("expected 'dynamic' or 'static'"),
                mode
            );
            std::process::exit(1);
        }
    }

    // One scratch directory for the whole run. `tempfile` places it under
    // TMPDIR when that is set (XSI, 88020-88022) and removes the whole tree on
    // drop, which is why no intermediate needs its own cleanup path.
    let scratch = tempfile::Builder::new().prefix("c17-").tempdir()?;

    let mut streams = StreamTable::new();
    // The object each operand contributes to the link, by operand index.
    // `None` means the operand contributes nothing (`-c`, an early-exit mode,
    // or an unrecognized file).
    let mut operand_objects: Vec<Option<String>> = vec![None; operands.len()];
    // CONSEQUENCES OF ERRORS (88185-88187): diagnose, keep compiling the
    // remaining operands, skip the link, exit non-zero.
    let mut failed = false;

    for (idx, op) in operands.iter().enumerate() {
        match op.kind {
            OperandKind::Unknown => {}
            OperandKind::Object => operand_objects[idx] = Some(op.path.clone()),
            OperandKind::Asm => {
                match assemble_operand(&op.path, &args, &target, scratch.path(), idx) {
                    Ok(Some(obj)) => operand_objects[idx] = Some(obj),
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("c17: {}: {}", op.path, e);
                        failed = true;
                    }
                }
            }
            OperandKind::Source => {
                let obj_name = source_object_name(&op.path, &args, scratch.path(), idx);
                match process_file(
                    &op.path,
                    &mut streams,
                    &args,
                    &target,
                    &obj_name,
                    scratch.path(),
                    idx,
                ) {
                    Ok(Compiled::Nothing) => {}
                    Ok(Compiled::Object { path, temporary }) => {
                        if temporary {
                            operand_objects[idx] = Some(path);
                        }
                    }
                    Err(e) => {
                        eprintln!("c17: {}: {}", op.path, e);
                        failed = true;
                    }
                }
                // Error state is global and sticky, so clear it before the
                // next operand — otherwise one bad file fails all the rest.
                diag::reset_counts();
            }
        }
    }

    let link_line = build_link_line(&scanned, &args, &operand_objects);

    let link_phase = !args.compile_only
        && !args.asm_only
        && !args.preprocess_only
        && !args.dump_tokens
        && !args.dump_ast
        && args.dump_ir.is_none();

    let has_object = link_line.iter().any(|i| matches!(i, LinkItem::Object(_)));

    if !failed && link_phase && has_object {
        let exe_file = args.output.clone().unwrap_or_else(|| "a.out".to_string());
        if let Err(e) = link_objects(&link_line, &exe_file, &args, &target) {
            eprintln!("c17: {}", e);
            failed = true;
        }
    }

    if failed {
        // process::exit skips Drop, so the scratch tree would survive.
        drop(scratch);
        std::process::exit(1);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Tests for is_object_file()
    // ========================================================================

    #[test]
    fn test_is_object_file_object() {
        assert!(is_object_file("foo.o"));
        assert!(is_object_file("/path/to/bar.o"));
    }

    #[test]
    fn test_is_object_file_static_archive() {
        assert!(is_object_file("libfoo.a"));
        assert!(is_object_file("/usr/lib/libbar.a"));
    }

    #[test]
    fn test_is_object_file_shared_object() {
        assert!(is_object_file("libfoo.so"));
        assert!(is_object_file("/usr/lib/libbar.so"));
    }

    #[test]
    fn test_is_object_file_dylib() {
        assert!(is_object_file("libfoo.dylib"));
        assert!(is_object_file("/usr/lib/libbar.dylib"));
    }

    #[test]
    fn test_is_object_file_versioned_so() {
        // Versioned shared objects like libz.so.1.3.1
        assert!(is_object_file("libz.so.1"));
        assert!(is_object_file("libz.so.1.3"));
        assert!(is_object_file("libz.so.1.3.1"));
        assert!(is_object_file("/usr/lib/libssl.so.3"));
    }

    #[test]
    fn test_is_object_file_negative() {
        assert!(!is_object_file("foo.c"));
        assert!(!is_object_file("bar.h"));
        assert!(!is_object_file("baz.txt"));
        assert!(!is_object_file("myso.txt")); // should not match .so
        assert!(!is_object_file("also.conf")); // should not match .so
    }

    // ========================================================================
    // Tests for is_source_file()
    // ========================================================================

    #[test]
    fn test_is_source_file_c() {
        assert!(is_source_file("foo.c"));
        assert!(is_source_file("/path/to/bar.c"));
    }

    #[test]
    fn test_is_source_file_preprocessed() {
        assert!(is_source_file("foo.i"));
        assert!(is_source_file("/path/to/bar.i"));
    }

    #[test]
    fn test_is_source_file_stdin() {
        assert!(is_source_file("-"));
    }

    #[test]
    fn test_is_source_file_negative() {
        assert!(!is_source_file("foo.o"));
        assert!(!is_source_file("bar.h"));
        assert!(!is_source_file("baz.cpp"));
    }

    // ========================================================================
    // Tests for preprocess_args()
    // ========================================================================

    fn run_preprocess(args: &[&str]) -> Vec<String> {
        let raw_args: Vec<String> = std::iter::once("c17".to_string())
            .chain(args.iter().map(|s| s.to_string()))
            .collect();
        preprocess_args_from(raw_args)
    }

    #[test]
    fn test_preprocess_std_is_forwarded_not_dropped() {
        // -std= used to be consumed and discarded here, which is why
        // __STDC_VERSION__ could not follow it (audit #P1/#X2).
        let result = run_preprocess(&["-std=c11", "foo.c"]);
        assert!(result.contains(&"--c17-std".to_string()));
        assert!(result.contains(&"c11".to_string()));
        assert!(!result.iter().any(|a| a.starts_with("-std=")));
        // The operand survives alongside it.
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_std_keeps_unknown_spelling_for_diagnosis() {
        // The rewriter does not validate; `main` reports the bad spelling so
        // the diagnostic can quote it.
        let result = run_preprocess(&["-std=c42", "foo.c"]);
        assert!(result.contains(&"--c17-std".to_string()));
        assert!(result.contains(&"c42".to_string()));
    }

    #[test]
    fn test_std_request_from_args() {
        let parse = |argv: &[&str]| {
            let argv = run_preprocess(argv);
            Args::parse_from(argv)
        };

        // No -std= at all is not a request; the language is C17 either way.
        assert_eq!(parse(&["foo.c"]).std_request(), Ok(None));
        assert_eq!(
            parse(&["-std=c17", "foo.c"]).std_request(),
            Ok(Some(StdRequest::C17))
        );
        // Recognized but older: accepted, and reported as not honoured.
        assert_eq!(
            parse(&["-std=c99", "foo.c"]).std_request(),
            Ok(Some(StdRequest::Older))
        );
        // A typo is still an error, not a silently ignored value.
        assert_eq!(parse(&["-std=c42", "foo.c"]).std_request(), Err("c42"));
    }

    #[test]
    fn test_preprocess_fpic_uppercase() {
        let result = run_preprocess(&["-fPIC", "foo.c"]);
        assert!(result.contains(&"--c17-fpic".to_string()));
        assert!(!result.contains(&"-fPIC".to_string()));
    }

    #[test]
    fn test_preprocess_fpic_lowercase() {
        let result = run_preprocess(&["-fpic", "foo.c"]);
        assert!(result.contains(&"--c17-fpic".to_string()));
        assert!(!result.contains(&"-fpic".to_string()));
    }

    #[test]
    fn test_preprocess_shared() {
        let result = run_preprocess(&["-shared", "foo.c"]);
        assert!(result.contains(&"--shared".to_string()));
    }

    #[test]
    fn test_preprocess_library_path() {
        let result = run_preprocess(&["-L/usr/local/lib", "foo.c"]);
        assert!(result.contains(&"-L".to_string()));
        assert!(result.contains(&"/usr/local/lib".to_string()));
    }

    #[test]
    fn test_preprocess_library() {
        let result = run_preprocess(&["-lz", "foo.c"]);
        assert!(result.contains(&"-l".to_string()));
        assert!(result.contains(&"z".to_string()));
    }

    #[test]
    fn test_preprocess_warning() {
        let result = run_preprocess(&["-Wall", "foo.c"]);
        assert!(result.contains(&"-W".to_string()));
        assert!(result.contains(&"all".to_string()));
    }

    #[test]
    fn test_preprocess_combined() {
        let result = run_preprocess(&["-fPIC", "-shared", "-lz", "-L.", "foo.c"]);
        assert!(result.contains(&"--c17-fpic".to_string()));
        assert!(result.contains(&"--shared".to_string()));
        assert!(result.contains(&"-l".to_string()));
        assert!(result.contains(&"z".to_string()));
        assert!(result.contains(&"-L".to_string()));
        assert!(result.contains(&".".to_string()));
    }

    // ========================================================================
    // Tests for silently-ignored flags
    // ========================================================================

    #[test]
    fn test_preprocess_fvisibility_ignored() {
        let result = run_preprocess(&["-fvisibility=hidden", "foo.c"]);
        assert!(!result.contains(&"-fvisibility=hidden".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_fstack_protector_ignored() {
        for flag in &[
            "-fstack-protector",
            "-fstack-protector-strong",
            "-fstack-protector-all",
        ] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(
                !result.contains(&flag.to_string()),
                "flag {} not ignored",
                flag
            );
            assert!(result.contains(&"foo.c".to_string()));
        }
    }

    #[test]
    fn test_preprocess_misc_f_flags_ignored() {
        for flag in &[
            "-fno-semantic-interposition",
            "-fno-reorder-blocks-and-partition",
            "-fno-plt",
            "-fno-common",
        ] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(
                !result.contains(&flag.to_string()),
                "flag {} not ignored",
                flag
            );
        }
    }

    #[test]
    fn test_preprocess_catchall_f_flags_ignored() {
        // Unknown -f flags should be silently ignored
        let result = run_preprocess(&["-funknown-flag", "foo.c"]);
        assert!(!result.contains(&"-funknown-flag".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_m_flags_unsupported() {
        let result = run_preprocess(&["-msse2", "foo.c"]);
        assert!(result.contains(&"--c17-unsupported-mflag=-msse2".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_pipe_ignored() {
        let result = run_preprocess(&["-pipe", "foo.c"]);
        assert!(!result.contains(&"-pipe".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_fpie() {
        let result = run_preprocess(&["-fPIE", "foo.c"]);
        assert!(result.contains(&"--c17-fpie".to_string()));
    }

    #[test]
    fn test_preprocess_fpie_lowercase() {
        let result = run_preprocess(&["-fpie", "foo.c"]);
        assert!(result.contains(&"--c17-fpie".to_string()));
    }

    #[test]
    fn test_preprocess_fno_pie() {
        let result = run_preprocess(&["-fno-pie", "foo.c"]);
        assert!(result.contains(&"--c17-fno-pie".to_string()));
    }

    #[test]
    fn test_preprocess_pie_linker_flag() {
        let result = run_preprocess(&["-pie", "foo.c"]);
        assert!(result.contains(&"--c17-fpie".to_string()));
        assert!(result.contains(&"--c17-linker-flag=-pie".to_string()));
    }

    #[test]
    fn test_preprocess_no_pie_linker_flag() {
        let result = run_preprocess(&["-no-pie", "foo.c"]);
        assert!(result.contains(&"--c17-fno-pie".to_string()));
        assert!(result.contains(&"--c17-linker-flag=-no-pie".to_string()));
    }

    // ========================================================================
    // Tests for linker passthrough flags
    // ========================================================================

    #[test]
    fn test_preprocess_wl_flags() {
        let result = run_preprocess(&["-Wl,-z,now", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-z".to_string()));
        assert!(result.contains(&"--c17-linker-flag=now".to_string()));
        assert!(!result.contains(&"-Wl,-z,now".to_string()));
    }

    #[test]
    fn test_preprocess_xlinker() {
        let result = run_preprocess(&["-Xlinker", "--hash-style=gnu", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=--hash-style=gnu".to_string()));
        assert!(!result.contains(&"-Xlinker".to_string()));
    }

    #[test]
    fn test_preprocess_pthread() {
        let result = run_preprocess(&["-pthread", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-pthread".to_string()));
        // Should also define _REENTRANT
        assert!(result.contains(&"-D".to_string()));
        assert!(result.contains(&"_REENTRANT".to_string()));
    }

    #[test]
    fn test_preprocess_rdynamic() {
        let result = run_preprocess(&["-rdynamic", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-rdynamic".to_string()));
    }

    #[test]
    fn test_preprocess_last_o_flag_wins() {
        // GCC convention: last -O flag wins
        let result = run_preprocess(&["-O3", "-Wall", "-O0"]);
        assert!(result.contains(&"-O0".to_string()));
        assert!(!result.contains(&"-O3".to_string()));

        let result = run_preprocess(&["-O0", "-O2"]);
        assert!(result.contains(&"-O2".to_string()));
        assert!(!result.contains(&"-O0".to_string()));

        // Standalone -O followed by -O2
        let result = run_preprocess(&["-O", "1", "-O2"]);
        assert!(result.contains(&"-O2".to_string()));
        assert!(!result.contains(&"-O1".to_string()));
    }

    #[test]
    fn test_preprocess_cpython_flags_combined() {
        // Simulate a typical CPython configure compiler probe
        let result = run_preprocess(&[
            "-fvisibility=hidden",
            "-fno-semantic-interposition",
            "-fstack-protector-strong",
            "-fno-plt",
            "-pipe",
            "-pthread",
            "-Wl,-z,now",
            "-rdynamic",
            "foo.c",
        ]);
        // Only foo.c and the passthrough flags should remain
        assert!(result.contains(&"foo.c".to_string()));
        // Silently-ignored flags should NOT appear
        assert!(!result.contains(&"-fvisibility=hidden".to_string()));
        assert!(!result.contains(&"-fno-semantic-interposition".to_string()));
        assert!(!result.contains(&"-fstack-protector-strong".to_string()));
        assert!(!result.contains(&"-fno-plt".to_string()));
        assert!(!result.contains(&"-pipe".to_string()));
        // Linker flags should be passed through
        assert!(result.iter().any(|a| a.starts_with("--c17-linker-flag=")));
    }
}
