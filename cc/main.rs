//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// c17 - A POSIX C17 compiler
//

#![recursion_limit = "512"]

use posixutils_cc::builtins;
use posixutils_cc::diag;
use posixutils_cc::f_options::{self, Effect};
use posixutils_cc::ir;
use posixutils_cc::linkargs;
use posixutils_cc::opt;
use posixutils_cc::parse;
use posixutils_cc::pipeline;
use posixutils_cc::prefix_map::{MapOption, PrefixMap, PrefixMaps};
use posixutils_cc::respfile;
use posixutils_cc::strings;
use posixutils_cc::symbol;
use posixutils_cc::target;
use posixutils_cc::token;
use posixutils_cc::types;
use posixutils_cc::warn_options::{self, Verdict};

use clap::Parser;
use gettextrs::{gettext, gettext_args};
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::Path;
use std::process::Command;

use strings::StringTable;
use symbol::SymbolTable;
use target::Os;
use target::{classify_std, StdRequest, Target};
use token::{
    preprocess_asm_file, preprocess_collecting, show_token, strip_bom, token_type_name,
    write_token, AsmPreprocessConfig, PreprocessConfig, TokenType,
};

// Runtime Library Selection

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

// CLI

#[derive(Parser)]
// `args_override_self`: a flag given twice is the last one winning, not an
// error. cc is driven by build systems that concatenate flag lists, so a
// command line carrying `-w` or `-g` twice is ordinary -- gcc and clang both
// take it -- and refusing it fails the build for a reason the user cannot see
// in their own makefile. Setting it on the command covers every argument at
// once, rather than repeating `overrides_with` on each of the thirty-odd
// flags and being wrong about the thirty-first.
#[command(
    version,
    args_override_self = true,
    about = gettext("c17 - compile standard C programs")
)]
struct Args {
    /// Not required with `-v` alone, which prints gcc's version banner.
    #[arg(required_unless_present_any = ["print_targets", "verbose"], help = gettext("Input files"))]
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

    /// Inhibit the `# <line> "<file>"` markers in `-E` output.
    ///
    /// Wanted whenever the result is read by something that is not a C
    /// compiler -- a linker script, an assembler, a kernel build's generated
    /// headers.
    #[arg(short = 'P', help = gettext("Do not emit line markers in preprocessed output"))]
    no_line_markers: bool,

    /// Dump every macro definition instead of the preprocessed text (`-dM`).
    #[arg(long = "dM", help = gettext("Dump macro definitions instead of output"))]
    dump_macros: bool,

    /// Write a make rule naming every header the source depends on, instead of
    /// compiling it (`-M`).
    #[arg(short = 'M', help = gettext("Write a make dependency rule instead of compiling"))]
    deps_only: bool,

    /// As `-M`, but leave out headers found on a system path (`-MM`).
    #[arg(long = "MM", help = gettext("Like -M, but omit system headers"))]
    deps_only_user: bool,

    /// Write the dependency rule *as well as* compiling (`-MD`).
    #[arg(long = "MD", help = gettext("Write a dependency rule and still compile"))]
    deps_side: bool,

    /// As `-MD`, but leave out system headers (`-MMD`).
    #[arg(long = "MMD", help = gettext("Like -MD, but omit system headers"))]
    deps_side_user: bool,

    /// Where the dependency rule goes (`-MF`). Defaults to stdout for `-M`
    /// and `-MM`, and to the source renamed `.d` for `-MD` and `-MMD`.
    #[arg(long = "MF", value_name = "file", help = gettext("Write dependencies to <file>"))]
    deps_file: Option<String>,

    /// The rule's target (`-MT`), in place of the source renamed `.o`.
    #[arg(
        long = "MT",
        value_name = "target",
        action = clap::ArgAction::Append,
        help = gettext("Name the dependency rule's target")
    )]
    deps_target: Vec<String>,

    /// Add a bare rule for each header, so removing one does not break the
    /// build until the makefile is regenerated (`-MP`).
    #[arg(long = "MP", help = gettext("Add a phony target for each dependency"))]
    deps_phony: bool,

    /// Process a file as if `#include "<file>"` were the first line
    /// (`-include`). Repeatable, applied in order.
    #[arg(
        long = "include",
        value_name = "file",
        action = clap::ArgAction::Append,
        help = gettext("Process <file> as if it were the first #include")
    )]
    pre_includes: Vec<String>,

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

    /// Re-root the target's system include directories under this prefix.
    ///
    /// Without it `--target` names the *host's* directories whatever target
    /// was asked for, so cross-compiling anything that includes a system
    /// header fails on the first header that header includes.
    #[arg(long = "sysroot", value_name = "dir", help = gettext("Use dir as the root of the target's system directories"))]
    sysroot: Option<String>,

    /// System include directories searched ahead of the target's own.
    #[arg(long = "isystem", action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add a system include path, searched before the target's own"))]
    isystem_paths: Vec<String>,

    /// System include directories searched behind the target's own.
    #[arg(long = "idirafter", action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add a system include path, searched after the target's own"))]
    idirafter_paths: Vec<String>,

    /// Annotate the generated assembly with what each instruction came from.
    #[arg(long = "fverbose-asm", help = gettext("Annotate the generated assembly"))]
    verbose_asm: bool,

    /// Disable all standard include paths (system and builtin), as gcc does.
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

    /// A member of gcc's `-fpic` family (`-fpic`, `-fPIC`, `-fpie`, `-fPIE`
    /// and their `-fno-` forms), carried in its gcc spelling by
    /// `preprocess_args_from`; the last one given wins.
    #[arg(
        long = "c17-pic",
        hide = true,
        value_name = "flag",
        value_parser = parse_pic_flag,
        overrides_with = "pic_flag"
    )]
    pic_flag: Option<target::PositionIndependence>,

    /// `-ftls-model=<model>`, validated by `preprocess_args_from`; the last
    /// one given wins.
    #[arg(
        long = "c17-tls-model",
        hide = true,
        value_name = "model",
        value_parser = parse_tls_model,
        overrides_with = "tls_model"
    )]
    tls_model: Option<target::TlsModel>,

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

    /// Optimization level: `-O0` none, `-O` / `-O1` / `-O2` / `-O3`, plus
    /// `-Os` (size) and `-Og` (debugging). `-O` alone means `-O1`.
    #[arg(short = 'O', default_value = "0", default_missing_value = "1",
          num_args = 0..=1, value_name = "level",
          value_parser = opt::Optimization::from_flag,
          help = gettext("Optimization level"))]
    opt_arg: opt::Optimization,

    /// `-fno-inline` / `-finline`, rewritten by `preprocess_args_from` so the
    /// two spellings share one option and the last occurrence wins.
    #[arg(
        long = "c17-inline",
        hide = true,
        value_name = "enabled",
        overrides_with = "inline_arg"
    )]
    inline_arg: Option<bool>,

    /// `-fsigned-char` / `-funsigned-char` and their `-fno-` inverses,
    /// rewritten by `preprocess_args_from` so the four spellings share one
    /// option and the last occurrence wins, as in GCC. Overrides the
    /// target's default for plain `char`.
    #[arg(
        long = "c17-plain-char",
        hide = true,
        value_name = "signedness",
        value_parser = parse_plain_char,
        overrides_with = "plain_char"
    )]
    plain_char: Option<target::CharSignedness>,

    /// `-fcf-protection[=level]` and `-fno-cf-protection`, rewritten by
    /// `preprocess_args_from` into one option, the last occurrence winning.
    #[arg(
        long = "c17-cf-protection",
        hide = true,
        value_name = "level",
        value_parser = parse_cf_protection,
        overrides_with = "cf_protection"
    )]
    cf_protection: Option<target::CfProtection>,

    /// `-fdebug-prefix-map=`, `-fmacro-prefix-map=` and `-ffile-prefix-map=`,
    /// each carried in its gcc spelling by `preprocess_args_from` so the three
    /// stay in one list in command-line order, which decides which map wins.
    #[arg(
        long = "c17-prefix-map",
        hide = true,
        action = clap::ArgAction::Append,
        value_name = "option",
        allow_hyphen_values = true,
        value_parser = parse_prefix_map
    )]
    prefix_maps: Vec<MapOption>,

    #[arg(short = 'W', action = clap::ArgAction::Append, value_name = "warning",
          num_args = 0..=1, default_missing_value = "extra", help = gettext("Warning flags (e.g., -Wall, -Wextra, -Wno-unused)"))]
    warnings: Vec<String>,

    #[arg(short = 'w', help = gettext("Suppress all warnings"))]
    no_warnings: bool,

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

    /// Accept implicit `int` and implicit function declarations as warnings.
    ///
    /// Both were removed by C99 and are errors here by default. This does not
    /// select a dialect -- see `diag::set_permissive`.
    #[arg(long = "fpermissive", help = gettext("Accept pre-C99 implicit int and implicit function declarations"))]
    fpermissive: bool,

    /// Disable builtin function recognition (GCC compatibility)
    /// c17 does not implicitly recognize standard library functions as builtins,
    /// so this flag is accepted for compatibility but has no effect.
    #[arg(long = "fno-builtin", help = gettext("Disable builtin function recognition"))]
    fno_builtin: bool,

    /// Disable specific builtin function (GCC compatibility)
    /// Accepts -fno-builtin-FUNC format via preprocess_args
    #[arg(long = "c17-fno-builtin-func", action = clap::ArgAction::Append, value_name = "func", hide = true)]
    fno_builtin_funcs: Vec<String>,

    /// GNU89 inline semantics: a plain `inline` emits an out-of-line body and
    /// `extern inline` does not, which is the opposite of C99's rule.
    #[arg(long = "fgnu89-inline", help = gettext("Use GNU89 inline semantics"))]
    fgnu89_inline: bool,

    /// Undo `-fgnu89-inline`. Accepted so the last flag on the line wins.
    #[arg(long = "fno-gnu89-inline", overrides_with = "fgnu89_inline", help = gettext("Use C99 inline semantics (default)"))]
    fno_gnu89_inline: bool,

    /// A libm function computed in place need not set `errno` for a domain
    /// error, so `sqrt` keeps no call for a negative argument.
    #[arg(long = "fno-math-errno", help = gettext("Do not set errno after math functions computed in place"))]
    fno_math_errno: bool,

    /// Undo `-fno-math-errno`: the default. Accepted so the last flag on the
    /// line wins.
    #[arg(long = "fmath-errno", overrides_with = "fno_math_errno", help = gettext("Set errno after math functions computed in place (default)"))]
    fmath_errno: bool,

    /// The program does not look at floating-point exception flags, so a
    /// comparison whose answer no operand can change -- `x > +Inf` -- may
    /// be folded although a NaN `x` would have raised `FE_INVALID`.
    #[arg(long = "fno-trapping-math", help = gettext("Assume floating-point operations do not raise exceptions the program observes"))]
    fno_trapping_math: bool,

    /// Undo `-fno-trapping-math`: the default. Accepted so the last flag on
    /// the line wins.
    #[arg(long = "ftrapping-math", overrides_with = "fno_trapping_math", help = gettext("Keep every floating-point exception the program can observe (default)"))]
    ftrapping_math: bool,

    /// Extra flags to pass through to the linker (set by preprocess_args)
    #[arg(long = "c17-linker-flag", action = clap::ArgAction::Append, value_name = "flag", hide = true)]
    linker_flags: Vec<String>,

    /// Machine (`-m`) flags captured by preprocess_args, judged against the
    /// target once it is known: see [`check_machine_flags`].
    #[arg(long = "c17-mflag", action = clap::ArgAction::Append, value_name = "flag", hide = true)]
    mflags: Vec<String>,

    /// `-fvisibility=`: the visibility of every definition that names none.
    #[arg(long = "c17-visibility", value_name = "visibility", hide = true)]
    default_visibility: Option<String>,

    /// `-x LANG` as it applied to each operand after it, as `LANG:path`
    /// (rewritten by `preprocess_args_from`); see [`Args::lang_of`].
    #[arg(long = "c17-x", action = clap::ArgAction::Append, value_name = "lang:path", hide = true)]
    lang_overrides: Vec<String>,

    /// The options accepted and ignored, each warned about once `-w` and
    /// `-Werror` are known (rewritten by `preprocess_args_from`).
    #[arg(long = "c17-ignored", action = clap::ArgAction::Append, value_name = "option",
          allow_hyphen_values = true, hide = true)]
    ignored_options: Vec<String>,

    /// The `-f` options c17 takes without doing what they ask, each warned
    /// about once `-w` and the `-W` options are known (rewritten by
    /// `preprocess_args_from`).
    #[arg(long = "c17-unsupported", action = clap::ArgAction::Append, value_name = "option",
          allow_hyphen_values = true, hide = true)]
    unsupported_options: Vec<String>,

    /// `-fstack-clash-protection`, last of it and `-fno-` winning (rewritten
    /// by `preprocess_args_from`): c17 probes no stack, and warns for each
    /// function whose stack gcc would probe.
    #[arg(long = "c17-stack-clash", hide = true)]
    stack_clash: bool,
}

/// The `-Wno-` name for the "`-std=` was not honoured" warning.
const STD_DIALECT_WARNING: &str = "c17-dialect";

/// Print a warning about the command line, unless `-w` turned warnings off.
///
/// These have no source position -- they are about the invocation, not a
/// translation unit -- so they cannot go through `diag`, which keys everything
/// on a `Position`. They still have to answer to `-w`, or its own help text
/// ("Suppress all warnings") is untrue.
fn driver_warning(msg: &str) {
    if diag::warnings_suppressed() {
        return;
    }
    eprintln!("c17: {}: {}", gettext("warning"), msg);
}

/// A linker input named in a run that links nothing (`-c`, `-S`, `-E`).
///
/// Said whatever the warning options: gcc's driver gives this one under `-w`
/// and leaves it a warning under `-Werror`.
fn unused_linker_input(path: &str) {
    eprintln!(
        "c17: {}: {}: {}",
        gettext("warning"),
        path,
        gettext("linker input file unused because linking not done")
    );
}

/// A command-line problem gcc refuses outright and c17 lets through with a
/// warning: an option it does not know, or one output named for several.
/// Under `-Werror` that leniency is withdrawn -- the warning is an error,
/// tagged as gcc tags a promoted one, and the run fails, as gcc's would --
/// because a configure probe adds `-Werror` exactly to find out whether the
/// compiler accepts what it is given. Answers whether it was an error.
///
/// The warnings gcc's driver gives itself stay [`driver_warning`]s: gcc's
/// `-Werror` does not reach them.
fn driver_leniency(msg: &str) -> bool {
    if diag::warnings_suppressed() || !diag::werror_all() {
        driver_warning(msg);
        return false;
    }
    eprintln!("c17: {}: {} [-Werror]", gettext("error"), msg);
    true
}

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
fn dump_ir(args: &Args, module: &ir::Module, types: &types::TypeTable, stage: &str) {
    if !should_dump_ir(args, stage) {
        return;
    }
    eprintln!("=== {} ===", stage);
    match &args.dump_ir_func {
        Some(name) => {
            for func in &module.functions {
                if func.name == *name {
                    print!("{}", func.display(types));
                }
            }
        }
        None => print!("{}", module.display(types)),
    }
}

/// Say which functions the optimizer's fixed-point loop gave up on, and
/// which passes were still changing them: their dumped IR is a snapshot, not
/// a fixed point.
fn note_unconverged(report: &opt::OptReport) {
    for c in &report.unconverged {
        eprintln!(
            "; note: '{}' did not reach a fixed point in {} iterations; still changing: {}",
            c.function,
            c.iterations,
            c.still_changing.join(", ")
        );
    }
}

/// The driver's dumps and `--stats`, at the pipeline's points of interest.
struct DriverObserver<'a> {
    args: &'a Args,
    path: &'a str,
}

impl pipeline::Observer for DriverObserver<'_> {
    fn parsed(&mut self, ast: &parse::ast::TranslationUnit) -> io::Result<bool> {
        if let Some(stage) = &self.args.dump_ir {
            if let Err(msg) = validate_dump_ir_stage(stage) {
                return Err(io::Error::new(io::ErrorKind::InvalidInput, msg));
            }
        }
        if self.args.dump_ast {
            println!("{:#?}", ast);
            return Ok(false);
        }
        Ok(true)
    }

    fn linearized(
        &mut self,
        module: &ir::Module,
        strings: &StringTable,
        types: &types::TypeTable,
        symbols: &SymbolTable,
    ) {
        if self.args.stats {
            print_stats(self.path, strings, types, symbols, module);
        }
    }

    fn stage(
        &mut self,
        stage: &str,
        module: &ir::Module,
        types: &types::TypeTable,
        report: Option<&opt::OptReport>,
    ) -> bool {
        let args = self.args;
        dump_ir(args, module, types, stage);
        if let Some(report) = report {
            if should_dump_ir(args, stage) {
                note_unconverged(report);
            }
        }
        match stage {
            "post-opt" => !(args.dump_ir.is_some() && !should_dump_ir(args, "post-lower")),
            "post-lower" => args.dump_ir.is_none(),
            _ => true,
        }
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

/// The system header search this invocation asked for.
fn system_search(args: &Args) -> token::preprocess::SystemSearch<'_> {
    token::preprocess::SystemSearch {
        sysroot: args.sysroot.as_deref(),
        isystem: &args.isystem_paths,
        idirafter: &args.idirafter_paths,
        no_std_inc: args.no_std_inc,
    }
}

/// Where `-E` writes.
///
/// `-o` is honored here, as gcc and clang do and as every build system that
/// runs `cc -E -o foo.i foo.c` assumes. POSIX leaves `-o` with `-E`
/// unspecified (88941-88942) and its own EXAMPLE redirects with `>` instead,
/// so this is a compatibility choice rather than a conformance one.
///
/// One sink serves the whole run: with several source operands the
/// preprocessed forms concatenate, as they do on stdout.
fn preprocess_sink(args: &Args) -> io::Result<Box<dyn Write>> {
    // Only `-E` may open `args.output`. Every other mode names its own output
    // downstream, and creating the file here would truncate the executable or
    // object a normal compile is about to write.
    match args.output.as_deref() {
        Some(path) if args.preprocess_only && path != "-" => {
            Ok(Box::new(BufWriter::new(File::create(path)?)))
        }
        _ => Ok(Box::new(BufWriter::new(io::stdout()))),
    }
}

/// Where one source operand's product goes.
///
/// The two travel together because the mode picks exactly one of them: an
/// early-exit `-E` writes to `preprocessed` and produces no object, and every
/// other mode fills `object` and never touches the stream.
struct Outputs<'a> {
    object: &'a ObjectName,
    preprocessed: &'a mut dyn Write,
}

impl Args {
    /// Whether any of the `-M` family was asked for.
    fn wants_dependencies(&self) -> bool {
        self.deps_only || self.deps_only_user || self.deps_side || self.deps_side_user
    }

    /// Whether the dependency rule *replaces* the compile (`-M`, `-MM`) rather
    /// than accompanying it (`-MD`, `-MMD`).
    fn dependencies_replace_output(&self) -> bool {
        self.deps_only || self.deps_only_user
    }

    /// Whether headers found on a system path are left out.
    fn dependencies_omit_system(&self) -> bool {
        self.deps_only_user || self.deps_side_user
    }
}

/// Preprocess an assembler operand under `-E` and write the text out.
///
/// The same pass `assemble_operand` runs before handing a `.S` to `as`, with
/// the result going to the preprocessed sink instead of a scratch file. A `.s`
/// has no directives to act on, so this is a copy for it -- which is also what
/// gcc does rather than skipping the operand.
fn preprocess_asm_operand(
    path: &str,
    args: &Args,
    target: &Target,
    out: &mut dyn Write,
) -> io::Result<()> {
    let content = strip_bom(&std::fs::read(path)?).to_vec();
    let config = AsmPreprocessConfig {
        optimization: args.optimization(),
        position: position_independence(args, target),
        isa: target::X86Isa::from_flags(&args.mflags),
        defines: &args.defines,
        undefines: &args.undefines,
        include_paths: &args.include_paths,
        search: system_search(args),
        no_std_inc: args.no_std_inc,
        macro_prefix_map: args.prefix_maps().macros,
    };
    let preprocessed = preprocess_asm_file(&content, target, path, &config).map_err(|e| {
        diag::reset_counts();
        io::Error::new(io::ErrorKind::InvalidData, e.to_string())
    })?;
    out.write_all(&preprocessed)?;
    out.flush()
}

/// Write the make rule for one translation unit.
///
/// The shape is gcc's, which is what a makefile's `include` expects:
/// `target: source header...`, wrapped at a sensible width with a trailing
/// backslash, and -- under `-MP` -- a bare `header:` rule for each
/// prerequisite so that deleting a header does not break the build before the
/// makefile is regenerated.
fn write_dependency_rule(
    args: &Args,
    source: &str,
    dependencies: &[(std::path::PathBuf, bool)],
) -> io::Result<()> {
    let target = dependency_target(args, source);

    let mut prerequisites = vec![escape_for_make(source)];
    for (path, is_system) in dependencies {
        if *is_system && args.dependencies_omit_system() {
            continue;
        }
        prerequisites.push(escape_for_make(&path.to_string_lossy()));
    }

    // gcc wraps near 80 columns; the exact column is cosmetic, the
    // backslash-newline is not.
    const WRAP: usize = 72;
    let mut rule = format!("{}:", target);
    let mut column = rule.len();
    for prereq in &prerequisites {
        if column + prereq.len() + 1 > WRAP {
            // gcc continues with " \\" then a single leading space, so the
            // space that separates prerequisites is the one already written.
            rule.push_str(" \\\n");
            column = 0;
        }
        rule.push(' ');
        rule.push_str(prereq);
        column += prereq.len() + 1;
    }
    rule.push('\n');

    if args.deps_phony {
        // Not for the source itself: it is not a header, and a rule for it
        // would shadow the real one.
        for prereq in prerequisites.iter().skip(1) {
            rule.push_str(prereq);
            rule.push_str(":\n");
        }
    }

    match dependency_sink(args, source) {
        Some(path) => std::fs::write(path, rule),
        None => {
            io::stdout().write_all(rule.as_bytes())?;
            io::stdout().flush()
        }
    }
}

/// A path as make will read it back.
///
/// The rule is written to be `include`d, so a character make gives meaning to
/// has to lose it: an unescaped space in a header path becomes two
/// prerequisites, and an unescaped `$` is variable-expanded when the `.d` is
/// read. gcc's escaping, probed: space and tab take a backslash, `#` takes a
/// backslash, and `$` is doubled.
fn escape_for_make(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    for c in path.chars() {
        match c {
            ' ' | '\t' | '#' | '\\' => {
                out.push('\\');
                out.push(c);
            }
            '$' => out.push_str("$$"),
            _ => out.push(c),
        }
    }
    out
}

/// The source's name with its directory dropped and its suffix replaced.
///
/// gcc derives both the default target and the default `.d` from the *basename*
/// -- `sub/dep.c` gives `dep.o` and `./dep.d`, not `sub/dep.o` -- which is the
/// same rule `-c` uses for the object file.
fn source_basename_with(source: &str, extension: &str) -> String {
    let stem = Path::new(source).file_stem().unwrap_or_default();
    format!("{}.{}", stem.to_string_lossy(), extension)
}

/// The rule's target.
///
/// `-MT` wins, joined by spaces when repeated, and is used verbatim: it is how
/// a caller writes a target make already understands, so escaping it would be
/// wrong. Otherwise the compiling forms take `-o` -- the object really being
/// built is what the rule is about -- and everything else falls back to the
/// source's basename with `.o`.
fn dependency_target(args: &Args, source: &str) -> String {
    if !args.deps_target.is_empty() {
        return args.deps_target.join(" ");
    }
    match &args.output {
        Some(out) if !args.dependencies_replace_output() && out != "-" => escape_for_make(out),
        _ => escape_for_make(&source_basename_with(source, "o")),
    }
}

/// Where the rule goes.
///
/// `-MF` wins outright. For `-M`/`-MM` the rule *is* the output, so `-o` names
/// it and stdout is the fallback. For `-MD`/`-MMD` it is a side output: `-o`'s
/// path with the suffix replaced -- keeping the directory, so `build/foo.o`
/// gives `build/foo.d` -- or the source's basename with `.d` when there is no
/// `-o`. Deriving it from the *source* path, as this did, wrote `./sub/dep.d`
/// for an object the build had asked to put in `build/`.
fn dependency_sink(args: &Args, source: &str) -> Option<std::path::PathBuf> {
    if let Some(file) = &args.deps_file {
        return Some(std::path::PathBuf::from(file));
    }
    let output = args.output.as_deref().filter(|p| *p != "-");
    if args.dependencies_replace_output() {
        return output.map(std::path::PathBuf::from);
    }
    Some(match output {
        Some(out) => Path::new(out).with_extension("d"),
        None => std::path::PathBuf::from(source_basename_with(source, "d")),
    })
}

/// Write the preprocessed token stream for `-E`.
///
/// STDOUT (88032-88038) requires the output to carry at least one
/// `# <line> "<file>"` line for each file processed via #include, so that a
/// consumer can attribute the text; RATIONALE (88370-88374) names makefile
/// dependency generation as the purpose.
fn emit_preprocessed(
    args: &Args,
    preprocessed: &[token::lexer::Token],
    outcome: &token::preprocess::PreprocessOutcome,
    strings: &StringTable,
    display_path: &str,
    stream_id: u16,
    out: &mut Outputs,
) -> io::Result<Compiled> {
    // Output preprocessed tokens.
    //
    // STDOUT (88032-88038) requires the -E output to carry at least one
    // `# <line> "<file>"` line for each file processed via #include, so
    // that a consumer can attribute the text; RATIONALE (88370-88374)
    // names makefile dependency generation as the purpose.
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
    // `-dM` asks what the macros are, not what the source becomes, so it
    // replaces the output rather than adding to it.
    if args.dump_macros {
        for line in &outcome.macro_definitions {
            writeln!(out.preprocessed, "{}", line)?;
        }
        out.preprocessed.flush()?;
        if diag::has_error() != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "preprocessing failed",
            ));
        }
        return Ok(Compiled::Nothing);
    }

    // `-P` asks for the text alone. Everything that would emit a marker
    // below checks this; a marker is never merely cosmetic, so each site
    // has to say what it does instead.
    let markers = !args.no_line_markers;
    if markers {
        writeln!(
            out.preprocessed,
            "# 1 \"{}\"",
            token::lexer::escape_c_string(display_path)
        )?;
    }
    let mut emitted_marker_for: Vec<u16> = vec![stream_id];
    let mut current_stream: Option<u16> = Some(stream_id);
    let mut at_line_start = true;
    // The source line the next output line stands for.
    //
    // Directives and blank lines produce no tokens, so without this the
    // output would close up the gaps they leave and every line after the
    // first `#define` would claim a number several too low. The markers
    // are the only record of where the text came from, so `c17 -E x.c -o
    // x.i` followed by `c17 -c x.i` would report an error in `x.c` at the
    // wrong line.
    let mut current_line: u32 = 1;
    let mut spelling: Vec<u8> = Vec::new();

    let mut iter = preprocessed.iter().peekable();
    while let Some(token) = iter.next() {
        if args.verbose {
            writeln!(
                out.preprocessed,
                "{:>4}:{:<3} {:12} {}",
                token.pos.line,
                token.pos.col,
                token_type_name(token.typ),
                show_token(token, strings)
            )?;
        } else {
            // A `#pragma pack` travels to the parser as a marker token
            // carrying an internal payload, and `show_token` spells that
            // payload `<PRAGMA pack:set:1>` -- a debug form, not C. It was
            // reaching the output, so `c17 -E` on any source using the
            // pragma produced a file that neither c17 nor gcc would
            // compile. Write the directive that produced it instead;
            // dropping it would lose the packing, which is the one thing
            // the marker exists to carry.
            let pragma = if token.typ == TokenType::Pragma {
                // `pack` and `scalar_storage_order` travel decoded, because
                // the parser acts on them; every other pragma travels as its
                // own text. Dropping the second kind is what made `c17 -E`
                // keep one pragma line out of five, so an `-E`/compile split
                // silently meant something different from compiling in one
                // step.
                match token::preprocess::LayoutPragma::from_token(token) {
                    Some(action) => Some(action.to_pragma_text()),
                    None => match token::preprocess::pragma_text(token) {
                        Some(text) => Some(text),
                        None => continue,
                    },
                }
            } else {
                None
            };

            let text = match &pragma {
                Some(directive) => directive.clone(),
                None => {
                    let text = show_token(token, strings);
                    // Skip stream markers (e.g., <STREAM_BEGIN>,
                    // <STREAM_END>) but NOT the '<' operator or '<=', etc.
                    if text.starts_with("<STREAM")
                        || text.starts_with("<ident")
                        || text.starts_with("<special")
                    {
                        continue;
                    }
                    text
                }
            };

            if current_stream != Some(token.pos.stream) {
                let (name, line, _) = diag::effective_position(token.pos);
                let returning = emitted_marker_for.contains(&token.pos.stream);
                if !returning {
                    emitted_marker_for.push(token.pos.stream);
                }
                if !at_line_start {
                    writeln!(out.preprocessed)?;
                    at_line_start = true;
                }
                if markers {
                    writeln!(
                        out.preprocessed,
                        "# {} \"{}\" {}",
                        line,
                        token::lexer::escape_c_string(&name),
                        if returning { 2 } else { 1 }
                    )?;
                }
                current_stream = Some(token.pos.stream);
                current_line = line;
            }

            // Put the token back on the line it came from. Every consumed
            // directive and every blank line is a gap the token stream
            // does not carry, so it has to be reopened here or the count
            // drifts for the rest of the file.
            if at_line_start {
                let (name, line, _) = diag::effective_position(token.pos);
                if line > current_line {
                    // GCC's threshold: a handful of blank lines is smaller
                    // than a marker, past that a marker is smaller.
                    const MAX_BLANK_RUN: u32 = 8;
                    if !markers {
                        // `-P` is asked for by things that are not C
                        // compilers, which want the text and nothing
                        // standing in for the lines that produced it. gcc
                        // collapses the run rather than padding it out.
                    } else if line - current_line <= MAX_BLANK_RUN {
                        for _ in 0..(line - current_line) {
                            writeln!(out.preprocessed)?;
                        }
                    } else {
                        writeln!(
                            out.preprocessed,
                            "# {} \"{}\"",
                            line,
                            token::lexer::escape_c_string(&name)
                        )?;
                    }
                    current_line = line;
                }
            }

            // A directive owns its line: it has to start one, and the text
            // after it has to start another.
            if pragma.is_some() {
                if !at_line_start {
                    writeln!(out.preprocessed)?;
                    current_line += 1;
                }
                writeln!(out.preprocessed, "{}", text)?;
                current_line += 1;
                at_line_start = true;
                continue;
            }

            // Byte for byte: a literal's payload holds one `char` per
            // source byte, so writing `text` re-encoded every byte >= 0x80
            // as two and `c17 -E` changed what the string held.
            spelling.clear();
            write_token(&mut spelling, token, strings);
            out.preprocessed.write_all(&spelling)?;
            at_line_start = false;
            // Check next token to determine separator
            if let Some(next) = iter.peek() {
                if next.pos.newline {
                    writeln!(out.preprocessed)?;
                    current_line += 1;
                    at_line_start = true;
                } else {
                    // Need a space if:
                    // 1. Original had whitespace, OR
                    // 2. Adjacent tokens would merge (both alphanumeric/underscore)
                    let next_text = show_token(next, strings);
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
                        write!(out.preprocessed, " ")?;
                    }
                }
            }
        }
    }
    // The output ends with a newline, but the last token already wrote one
    // unless it was mid-line. Writing unconditionally left a trailing blank
    // line that gcc does not produce.
    if !args.verbose && !at_line_start {
        writeln!(out.preprocessed)?;
    }
    // The sink may be a file, and a BufWriter's Drop discards errors.
    out.preprocessed.flush()?;
    // Check for preprocessor errors (e.g., #error directive)
    if diag::has_error() != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "preprocessing failed",
        ));
    }
    Ok(Compiled::Nothing)
}

fn process_file(
    path: &str,
    args: &Args,
    target: &Target,
    out: &mut Outputs,
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

    // POSIX 87981-87983: a `.i` operand is the output of `c17 -E`, and the
    // processing that produced it "shall not be repeated when the file is
    // compiled". Phases 1 and 2 are part of that processing, so neither runs
    // here; phase 4 is narrowed to GCC's allowlist inside the preprocessor.
    let preprocessed = args.lang_of(path) == Lang::Preprocessed;

    // Create shared string table for identifier interning
    let mut strings = StringTable::new();

    let (tokens, stream_id) = pipeline::source_tokens(
        &buffer,
        display_path,
        args.trigraphs,
        preprocessed,
        &mut strings,
    );

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

    let prefix_maps = args.prefix_maps();

    // Preprocess (may add new identifiers from included files)
    let (preprocessed, outcome) = preprocess_collecting(
        tokens,
        target,
        &mut strings,
        path,
        &PreprocessConfig {
            defines: &args.defines,
            undefines: &args.undefines,
            include_paths: &args.include_paths,
            search: system_search(args),
            no_std_inc: args.no_std_inc,
            no_builtin_inc: args.no_builtin_inc,
            trigraphs: args.trigraphs,
            preprocessed,
            pre_includes: &args.pre_includes,
            dump_macros: args.dump_macros,
            collect_dependencies: args.wants_dependencies(),
            optimization: args.optimization(),
            position: position_independence(args, target),
            isa: target::X86Isa::from_flags(&args.mflags),
            macro_prefix_map: prefix_maps.macros,
        },
    );

    // The dependency rule is a property of preprocessing, so it is written
    // here whichever form asked for it -- before `-E` decides what to print,
    // and before compilation, which `-MD`/`-MMD` do not suppress.
    if args.wants_dependencies() {
        write_dependency_rule(args, display_path, &outcome.dependencies)?;
        if args.dependencies_replace_output() {
            // `-M` and `-MM` produce the rule *instead of* anything else --
            // but a rule built from a translation unit that did not
            // preprocess is incomplete, and exiting 0 would have a makefile
            // record it as authoritative.
            if diag::has_error() != 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "preprocessing failed",
                ));
            }
            return Ok(Compiled::Nothing);
        }
    }

    if args.preprocess_only {
        return emit_preprocessed(
            args,
            &preprocessed,
            &outcome,
            &strings,
            display_path,
            stream_id,
            out,
        );
    }

    // A shared object gets shared-object code whatever the `-fpic` family
    // said. gcc compiles `-shared` alone as a PIE, which a shared object
    // cannot always hold.
    let position = position_independence(args, target);
    let tls = target::TlsPolicy {
        shared_code: producing_shared(args) || position.is_shared_code(),
        floor: args.tls_model.unwrap_or_default(),
    };
    let codegen_opts = pipeline::CodegenOptions {
        optimization: args.optimization(),
        math_errno: !args.fno_math_errno,
        debug: args.debug > 0,
        trapping_math: !args.fno_trapping_math,
        default_visibility: args.default_visibility.as_deref(),
        tls,
        pic: producing_shared(args) || position.is_pic(),
        unwind_tables: !args.no_unwind_tables,
        verbose_asm: args.verbose_asm,
        cf_protection: args.cf_protection.unwrap_or_default(),
        stack_clash: args.stack_clash,
        source_name: path,
        debug_prefix_map: &prefix_maps.debug,
    };
    let compiled = pipeline::compile_tokens(
        preprocessed,
        &strings,
        target,
        &codegen_opts,
        &mut DriverObserver { args, path },
    )?;
    let Some(asm) = compiled else {
        return Ok(Compiled::Nothing);
    };

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
                eprintln!("{}: {}", gettext("wrote assembly to"), asm_file);
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
    let (obj_file, temporary) = match out.object {
        ObjectName::Keep(p) => (p.clone(), false),
        ObjectName::Temp(p) => (p.clone(), true),
    };

    let status = AssemblerCommand::new(
        target.os,
        args.debug > 0,
        &prefix_maps.debug,
        &temp_asm,
        &obj_file,
    )
    .command()
    .status()?;

    let _ = std::fs::remove_file(&temp_asm);

    if !status.success() {
        return Err(io::Error::other("assembler failed"));
    }

    if args.verbose && !temporary {
        eprintln!("{}: {}", gettext("wrote object file to"), obj_file);
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
    /// An argument for the host driver's link step -- `-Wl,...`, `-pthread`,
    /// `-rdynamic` -- in its place among the others: `-Wl,--whole-archive`
    /// governs the archives after it.
    Flag(String),
}

/// How `-s` removes the symbol table from the linked executable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StripBy {
    /// The linker does it: `-s` is passed through to GNU ld.
    LinkerFlag,
    /// `strip` runs on the output: Apple's ld64 accepts `-s` but ignores it
    /// as obsolete, leaving every symbol in place.
    StripTool,
}

impl StripBy {
    fn for_os(os: Os) -> Self {
        match os {
            Os::MacOS => StripBy::StripTool,
            Os::Linux | Os::FreeBSD => StripBy::LinkerFlag,
        }
    }
}

/// The host driver option that leads the link line and says what kind of
/// file it makes.
///
/// A static link is never `-pie`: gcc's answer to `-pie -static` is a
/// fixed-address static executable, and c17 compiles position-independent
/// code by default, which is what makes `-pie` the default here. Only
/// `-static-pie` asks for both.
///
/// `None` for an executable on macOS: a Mach-O executable is always
/// position independent there, and Apple's driver answers `-pie` and
/// `-no-pie` alike with "argument unused during compilation". A `-static`,
/// `-static-pie`, `-pie` or `-no-pie` on the command line still reaches it
/// as written, among the other linker flags.
fn link_mode_flag(args: &Args, target: &Target) -> Option<&'static str> {
    let has = |flag: &str| args.linker_flags.iter().any(|f| f == flag);
    Some(if producing_shared(args) {
        "-shared"
    } else if target.os == Os::MacOS {
        return None;
    } else if has("-static-pie") {
        "-static-pie"
    } else if has("-static") || !link_pie(args, target) {
        "-no-pie"
    } else {
        "-pie"
    })
}

/// Whether an executable is linked as a PIE: as the last of `-pie` and
/// `-no-pie` says, or else as the code was compiled. gcc links a PIE by
/// default even after `-fno-pic`, which works only if the objects happen to
/// be position independent; c17 links position-dependent code into the
/// executable it was compiled for.
fn link_pie(args: &Args, target: &Target) -> bool {
    match args
        .linker_flags
        .iter()
        .rev()
        .find(|f| *f == "-pie" || *f == "-no-pie")
    {
        Some(flag) => flag == "-pie",
        None => position_independence(args, target).is_pic(),
    }
}

/// Link `link_line` into `exe_file`, preserving the order given.
fn link_objects(
    link_line: &[LinkItem],
    exe_file: &str,
    args: &Args,
    target: &Target,
) -> io::Result<()> {
    let mut link_cmd = linkargs::host_driver();
    if let Some(mode) = link_mode_flag(args, target) {
        link_cmd.arg(mode);
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
            driver_warning(&gettext(
                "-B static: this platform's linker cannot prefer archives",
            ));
        }
        Some("dynamic") if gnu_binding => {
            link_cmd.arg("-Wl,-Bdynamic");
        }
        _ => {}
    }

    // Emit the recovered link line in argument order: a library is searched
    // where its name was encountered, not after every object.
    //
    // `-L` directories accumulate as we go, because a standard library is
    // resolved against the paths that precede its `-l` and no others
    // (88925-88929).
    let mut lib_paths_so_far: Vec<String> = Vec::new();
    for item in link_line {
        match item {
            LinkItem::Object(p) => {
                link_cmd.arg(p);
            }
            LinkItem::LibPath(d) => {
                lib_paths_so_far.push(d.clone());
                link_cmd.arg(format!("-L{}", d));
            }
            LinkItem::Library(l) => {
                // One of the seven POSIX standard libraries that this host
                // does not ship is satisfied by libc, which the host driver
                // links anyway -- so the name is dropped rather than passed on
                // to fail. Every other name goes through untouched.
                if linkargs::drop_standard_library(l, &lib_paths_so_far) {
                    if args.verbose {
                        eprintln!(
                            "c17: {}: -l {}",
                            gettext("standard library provided by the C library"),
                            l
                        );
                    }
                    continue;
                }
                link_cmd.arg(format!("-l{}", l));
            }
            LinkItem::RunPath(d) => {
                link_cmd.arg(format!("-Wl,-rpath,{}", d));
            }
            LinkItem::Flag(f) => {
                link_cmd.arg(f);
            }
        }
    }

    // -B governs only the libraries named on the command line. The host driver
    // appends its own (libc, libgcc_s) after everything here, and those are not
    // usually available as archives, so binding is restored before them.
    if args.binding.as_deref() == Some("static") && gnu_binding {
        link_cmd.arg("-Wl,-Bdynamic");
    }

    let strip = args.strip.then(|| StripBy::for_os(target.os));
    if strip == Some(StripBy::LinkerFlag) {
        link_cmd.arg("-s");
    }

    if !link_cmd.status()?.success() {
        return Err(io::Error::other("linker failed"));
    }

    if strip == Some(StripBy::StripTool) && !Command::new("strip").arg(exe_file).status()?.success()
    {
        return Err(io::Error::other("strip failed"));
    }

    if args.verbose {
        eprintln!("{}: {}", gettext("linked to"), exe_file);
    }
    Ok(())
}

/// Whether `s` looks like the argument of `-O`, so that `-O s` is read as a
/// level rather than as a source file operand.
///
/// Wider than [`opt::Optimization::from_flag`] on purpose: `fast` and `z` are
/// recognised here so they reach the parser and are turned down by name,
/// instead of being mistaken for a file and producing a confusing error.
impl Args {
    /// The optimization the command line asked for: `-O...` combined with
    /// `-f[no-]inline`.
    ///
    /// What `path` is to be read as: what the last `-x` before it said, or
    /// else what its suffix says.
    fn lang_of(&self, path: &str) -> Lang {
        let overridden = self.lang_overrides.iter().rev().find_map(|o| {
            o.split_once(':')
                .filter(|(_, p)| *p == path)
                .map(|(l, _)| l)
        });
        match overridden {
            Some("c") => Lang::C,
            Some("cpp-output") => Lang::Preprocessed,
            Some("assembler") => Lang::Asm,
            Some(_) => Lang::AsmCpp,
            None => lang_by_suffix(path),
        }
    }

    /// One value, so that what the optimizer does and what `__OPTIMIZE__`,
    /// `__OPTIMIZE_SIZE__` and `__NO_INLINE__` claim cannot drift apart.
    fn optimization(&self) -> opt::Optimization {
        let mut opt = self.opt_arg;
        if let Some(enabled) = self.inline_arg {
            opt.set_inlining(enabled);
        }
        opt
    }

    /// The maps the `-f*-prefix-map=` options build, in command-line order.
    fn prefix_maps(&self) -> PrefixMaps {
        PrefixMaps::from_options(&self.prefix_maps)
    }
}

/// The value of the internal `--c17-plain-char` option.
fn parse_plain_char(s: &str) -> Result<target::CharSignedness, String> {
    match s {
        "signed" => Ok(target::CharSignedness::Signed),
        "unsigned" => Ok(target::CharSignedness::Unsigned),
        _ => Err(format!("invalid plain char signedness '{s}'")),
    }
}

/// The value of the internal `--c17-cf-protection` option: a level already
/// validated by `preprocess_args_from`.
fn parse_cf_protection(s: &str) -> Result<target::CfProtection, String> {
    target::CfProtection::from_level(s).ok_or_else(|| format!("invalid cf-protection level '{s}'"))
}

/// The value of the internal `--c17-pic` option: a member of the `-fpic`
/// family, in its gcc spelling.
fn parse_pic_flag(s: &str) -> Result<target::PositionIndependence, String> {
    target::PositionIndependence::from_flag(s).ok_or_else(|| format!("not a -fpic option: '{s}'"))
}

/// The value of the internal `--c17-tls-model` option: a model name already
/// validated by `preprocess_args_from`.
fn parse_tls_model(s: &str) -> Result<target::TlsModel, String> {
    target::TlsModel::from_name(s).ok_or_else(|| format!("unknown TLS model '{s}'"))
}

/// The model a `-ftls-model=` option names, as the value of
/// `--c17-tls-model`. A name gcc does not know is an error, in its words.
fn tls_model_name(model: &str) -> &str {
    if model.is_empty() {
        eprintln!("c17: error: missing argument to '-ftls-model='");
        std::process::exit(1);
    }
    if target::TlsModel::from_name(model).is_none() {
        eprintln!("c17: error: unknown TLS model '{model}'");
        eprintln!(
            "c17: note: valid arguments to '-ftls-model=' are: {}",
            target::TlsModel::NAMES
        );
        std::process::exit(1);
    }
    model
}

/// The value of the internal `--c17-prefix-map` option: a prefix-map
/// option in its gcc spelling, already validated by `preprocess_args_from`.
fn parse_prefix_map(s: &str) -> Result<MapOption, String> {
    MapOption::parse(s).unwrap_or_else(|| Err(format!("not a prefix map: '{s}'")))
}

/// The plain-`char` signedness a GCC `-f` flag selects, as the value of
/// `--c17-plain-char`: `-fno-signed-char` means unsigned and
/// `-fno-unsigned-char` means signed.
fn plain_char_flag(arg: &str) -> Option<&'static str> {
    match arg {
        "-fsigned-char" | "-fno-unsigned-char" => Some("signed"),
        "-funsigned-char" | "-fno-signed-char" => Some("unsigned"),
        _ => None,
    }
}

/// The level a `-fcf-protection` spelling selects, as the value of
/// `--c17-cf-protection`. The bare flag is gcc's `full`, and
/// `-fno-cf-protection` is `none`. A level gcc does not know is an error, in
/// its words.
fn cf_protection_level(arg: &str) -> &str {
    let level = match arg {
        "-fno-cf-protection" => "none",
        "-fcf-protection" => "full",
        _ => match arg.strip_prefix("-fcf-protection=") {
            Some(level) => level,
            None => {
                eprintln!("c17: {}: {}", gettext("unrecognized option"), arg);
                std::process::exit(1);
            }
        },
    };
    if target::CfProtection::from_level(level).is_none() {
        eprintln!(
            "c17: {}: {}",
            gettext("unknown Control-Flow Protection Level"),
            level
        );
        std::process::exit(1);
    }
    level
}

fn is_valid_opt_level(s: &str) -> bool {
    matches!(s, "0" | "1" | "2" | "3" | "s" | "z" | "fast" | "g")
}

/// Preprocess this process's command-line arguments for gcc compatibility.
///
/// `@file` response files are expanded first, so the rewriting below, clap and
/// `linkargs::scan` all read the same, complete argument vector.
fn preprocess_args() -> Vec<String> {
    match respfile::expand(std::env::args().collect()) {
        Ok(argv) => preprocess_args_from(argv),
        Err(e) => {
            eprintln!("c17: {e}");
            std::process::exit(1);
        }
    }
}

/// Preprocess command-line arguments for gcc compatibility.
/// - Converts -Wall → -W all, -Wextra → -W extra, etc.
/// - Handles -O flag: standalone -O followed by non-level becomes -O1
///
/// Takes the raw argument vector rather than reading the environment so the
/// unit tests exercise this exact function.
fn preprocess_args_from(raw_args: Vec<String>) -> Vec<String> {
    let mut result = Vec::with_capacity(raw_args.len());
    let mut i = 0;
    let mut o_flag_idx: Option<usize> = None; // index into result of the -O flag
    let mut std_flag_idx: Option<usize> = None; // index into result of the -std= value

    // `-x LANG`: the language every operand after it is read as, until the
    // next `-x` (`none` restores reading by suffix).
    let mut lang: Option<&'static str> = None;
    // The operands `-x` applied to, appended once the scan is done: an
    // option's value is indistinguishable from an operand here, and a marker
    // pushed in place would separate `-o` from its file.
    let mut lang_overrides = Vec::new();
    // `-g` and its levels, last one wins: `-g3 -g0` is no debug information.
    let mut debug: Option<bool> = None;
    // `-fsignaling-nans`, last one wins.
    let mut signaling_nans = false;
    // The options accepted and ignored with a warning, which waits for the
    // parse: `-w` and `-Werror` decide what it is, wherever they stand.
    let mut ignored = Vec::new();
    // The `-f` options taken without their effect, by what each asks for
    // (`f_options::family`): a later one of a family replaces an earlier
    // one, and `-fno-<family>` withdraws it. Warned about after the parse.
    let mut unsupported: Vec<(String, String)> = Vec::new();
    // `-fstack-clash-protection`, last one wins.
    let mut stack_clash = false;
    // The `-W<name>` and `-f<name>` options refused as gcc refuses them: by
    // its driver, and -- only if the driver let everything through -- by
    // its compiler.
    let mut driver_errors = Vec::new();
    let mut werror_errors = Vec::new();

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
            // -O0, -O1, -O2, -O3, -Os, -Og and the ones we refuse — last wins,
            // and the refusal happens in the parser so it can name the flag.
            if let Some(idx) = o_flag_idx {
                result[idx] = arg.clone();
            } else {
                o_flag_idx = Some(result.len());
                result.push(arg.clone());
            }
            i += 1;
        } else if arg.starts_with("-W") && arg.len() > 2 && !arg.starts_with("-Wl,") {
            // -Wall → -W all, -Wextra → -W extra, etc. -- once the name is
            // known to be one gcc would take.
            match warn_options::classify(&arg[2..]) {
                Verdict::DriverError(lines) => driver_errors.extend(lines),
                Verdict::CompilerError(line) => werror_errors.push(line),
                Verdict::Known(_) | Verdict::PassThrough | Verdict::UnknownNegation => {}
            }
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
        } else if arg == "-ansi" {
            // gcc's spelling of `-std=c90`, reported the same way.
            if let Some(idx) = std_flag_idx {
                result[idx] = "c90".to_string();
            } else {
                result.push("--c17-std".to_string());
                std_flag_idx = Some(result.len());
                result.push("c90".to_string());
            }
            i += 1;
        } else if arg == "-pedantic" || arg == "-pedantic-errors" {
            // -pedantic → -W pedantic, -pedantic-errors → -W pedantic-errors:
            // among the `-W` options, so `diag::Pedantic` folds them in
            // command-line order with `-Wpedantic` and `-Wno-pedantic`.
            result.push("-W".to_string());
            result.push(arg[1..].to_string());
            i += 1;
        } else if arg == "-x" || (arg.starts_with("-x") && arg.len() > 2) {
            let (name, used) = match arg.strip_prefix("-x").filter(|n| !n.is_empty()) {
                Some(name) => (name.to_string(), 1),
                None => (raw_args.get(i + 1).cloned().unwrap_or_default(), 2),
            };
            lang = match source_language(&name) {
                Some(l) => l,
                None => {
                    eprintln!(
                        "c17: {}: {}",
                        gettext("language not recognized"),
                        if name.is_empty() { "-x" } else { &name }
                    );
                    std::process::exit(1);
                }
            };
            i += used;
        } else if let Some(level) = debug_flag_level(arg) {
            debug = level.or(debug);
            i += 1;
        } else if arg.starts_with("-g") && arg.len() > 2 {
            // Debug-format and debug-content tuning: c17 emits one kind of
            // DWARF, so these change nothing it could honour.
            if !is_known_ignorable_g_flag(arg) {
                ignored.push(format!("--c17-ignored={arg}"));
            }
            i += 1;
        } else if target::PositionIndependence::from_flag(arg).is_some() {
            // The `-fpic` family, whose last member wins.
            result.push(format!("--c17-pic={arg}"));
            i += 1;
        } else if let Some(model) = arg.strip_prefix("-ftls-model=") {
            result.push(format!("--c17-tls-model={}", tls_model_name(model)));
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
        } else if arg.starts_with("-m") && arg.len() > 2 {
            // Machine flags are judged once the target is known.
            result.push(format!("--c17-mflag={}", arg));
            i += 1;
        } else if let Some(how) = arg.strip_prefix("-fvisibility=") {
            // The visibility a definition gets when it names none; see
            // `ir::Module::apply_default_visibility`. gcc rejects a value it
            // does not know, and so does this.
            if !matches!(how, "default" | "hidden" | "internal" | "protected") {
                eprintln!("c17: {}: {}", gettext("unrecognized visibility value"), how);
                std::process::exit(1);
            }
            result.push(format!("--c17-visibility={}", how));
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
        } else if arg == "-fno-inline" || arg == "-finline" {
            // Both spellings become one option so clap's last-wins applies,
            // as it does in GCC. Note `-fno-inline-functions` is a *different*
            // flag -- it only stops functions not declared `inline` from being
            // inlined, and does not define `__NO_INLINE__` -- so it falls
            // through to the catch-all below, accepted and ignored.
            result.push(format!("--c17-inline={}", arg == "-finline"));
            i += 1;
        } else if arg == "-fno-cf-protection" || arg.starts_with("-fcf-protection") {
            result.push(format!("--c17-cf-protection={}", cf_protection_level(arg)));
            i += 1;
        } else if let Some(signedness) = plain_char_flag(arg) {
            result.push(format!("--c17-plain-char={signedness}"));
            i += 1;
        } else if let Some(map) = MapOption::parse(arg) {
            // Diagnosed here, in gcc's words, rather than by clap, whose
            // message would name the internal spelling.
            if let Err(msg) = map {
                eprintln!("c17: {}: {}", gettext("error"), msg);
                std::process::exit(1);
            }
            result.push(format!("--c17-prefix-map={arg}"));
            i += 1;
        } else if arg == "-fverbose-asm" {
            result.push("--fverbose-asm".to_string());
            i += 1;
        } else if arg == "-fpermissive" {
            result.push("--fpermissive".to_string());
            i += 1;
        } else if arg == "-fsignaling-nans" || arg == "-fno-signaling-nans" {
            // Nothing c17 folds assumes a NaN is quiet, so the optimizer is
            // already what gcc's is under `-fsignaling-nans`: an identity
            // like `x * 1.0 -> x`, which would hand back a signalling `x`
            // where the multiplication quiets it, is not one it makes. What
            // the flag still changes is gcc's `__SUPPORT_SNAN__`, which
            // glibc's <math.h> and <fenv.h> read.
            signaling_nans = arg == "-fsignaling-nans";
            i += 1;
        } else if arg == "-fgnu89-inline"
            || arg == "-fno-gnu89-inline"
            || arg == "-fmath-errno"
            || arg == "-fno-math-errno"
            || arg == "-ftrapping-math"
            || arg == "-fno-trapping-math"
        {
            result.push(format!("-{arg}"));
            i += 1;
        } else if arg == "-fstack-clash-protection" || arg == "-fno-stack-clash-protection" {
            stack_clash = arg == "-fstack-clash-protection";
            i += 1;
        } else if arg.starts_with("-fuse-ld=")
            && f_options::classify(&arg[2..]) == f_options::Verdict::Known(Effect::Implemented)
        {
            // Which linker the host driver runs, so it goes to the link.
            result.push(format!("--c17-linker-flag={arg}"));
            i += 1;
        } else if let Some(name) = arg.strip_prefix("-f") {
            // Every other `-f` option, as `f_options` classifies it: taken
            // in silence when c17's output already is what it asks for, taken
            // with a warning when its effect is missing, and refused in gcc's
            // words when gcc would not know it.
            match f_options::classify(name) {
                f_options::Verdict::Known(Effect::Accepted(_)) => {}
                f_options::Verdict::Known(Effect::Unsupported) => {
                    let family = f_options::family(name);
                    unsupported.retain(|(f, _)| f != family);
                    unsupported.push((family.to_string(), arg.clone()));
                }
                f_options::Verdict::Known(Effect::Implemented) => {
                    debug_assert!(false, "{arg} is implemented, so parsed above");
                }
                f_options::Verdict::Error(lines) => driver_errors.extend(lines),
            }
            // `-fno-<family>` withdraws an earlier request.
            if let Some(family) = name.strip_prefix("no-") {
                unsupported.retain(|(f, _)| f != family);
            }
            i += 1;
        } else if arg == "--param" || arg.starts_with("--param=") {
            // `--param name=value` tunes a gcc heuristic -- inlining limits,
            // GC thresholds, unrolling budgets. Every one of them names an
            // internal gcc parameter, so there is nothing for c17 to honour
            // and nothing it could get wrong by ignoring. It still has to be
            // *consumed*: the separated spelling puts the setting in the next
            // argument, and leaving that behind made clap read `ggc-min-expand=1`
            // as a source file.
            if arg == "--param" {
                i += 1; // the setting travels separately
            }
            i += 1;
        } else if arg == "-nostdinc" || arg == "-nobuiltininc" {
            // gcc spells these with one dash; clap declares them long-only.
            result.push(format!("-{}", arg));
            i += 1;
        } else if matches!(arg.as_str(), "-MM" | "-MD" | "-MMD" | "-MP") {
            // gcc spells these with one dash; clap would read them as short
            // clusters.
            result.push(format!("-{}", arg));
            i += 1;
        } else if arg == "-MF" || arg == "-MT" {
            result.push(format!("-{}", arg));
            if let Some(v) = raw_args.get(i + 1) {
                result.push(v.clone());
                i += 2;
            } else {
                i += 1;
            }
        } else if arg == "-dM" {
            // One dash in gcc; clap would read it as the short cluster `-d -M`.
            result.push("--dM".to_string());
            i += 1;
        } else if arg == "-include" {
            // gcc spells it with one dash; clap would read that as the short
            // cluster `-i -n -c ...` and reject it.
            result.push("--include".to_string());
            if let Some(v) = raw_args.get(i + 1) {
                result.push(v.clone());
                i += 2;
            } else {
                i += 1;
            }
        } else if arg == "-isystem" || arg == "-idirafter" || arg == "--sysroot" {
            // Value options gcc spells with one dash. `--sysroot` is already
            // two, but takes its value as a separate word here either way.
            let long = if arg.starts_with("--") {
                arg.to_string()
            } else {
                format!("-{}", arg)
            };
            result.push(long);
            if let Some(v) = raw_args.get(i + 1) {
                result.push(v.clone());
                i += 2;
            } else {
                i += 1;
            }
        } else if let Some(dir) = arg.strip_prefix("--sysroot=") {
            result.push("--sysroot".to_string());
            result.push(dir.to_string());
            i += 1;
        } else if arg == "-p" || arg == "-pg" {
            // Profiling flags - silently ignore (c17 doesn't support profiling)
            i += 1;
        } else if arg == "-pipe" {
            // Misc GCC flags - silently ignore
            i += 1;
        } else if arg == "-pie" || arg == "-no-pie" {
            // Link options only: what the code is compiled as is the `-fpic`
            // family's business, as it is gcc's. See `link_mode_flag`.
            result.push(format!("--c17-linker-flag={arg}"));
            i += 1;
        } else if arg.starts_with("-Wl,") {
            // Handed to the host driver as written, which is what splits the
            // commas and gives each piece to the linker. Splitting here made
            // `-Wl,--as-needed` a driver option `--as-needed` it rejects, and
            // `-Wl,-soname,libx.so` two unrelated arguments.
            result.push(format!("--c17-linker-flag={}", arg));
            i += 1;
        } else if arg == "-Xlinker" {
            // -Xlinker <arg> -> the same pair, for the host driver
            if i + 1 < raw_args.len() {
                result.push("--c17-linker-flag=-Xlinker".to_string());
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
        } else if arg == "-static" || arg == "-static-pie" {
            // For the link step, which reads them to choose its leading
            // option: see `link_mode_flag`. clap would read `-static` as the
            // short cluster `-s -t -a ...`.
            result.push(format!("--c17-linker-flag={arg}"));
            i += 1;
        } else if let Some(status) = answer_driver_query(&raw_args[i..], &raw_args) {
            std::process::exit(status);
        } else {
            // An operand, or an option's value -- the two cannot be told apart
            // here, and recording a language for a value is harmless, since
            // only operands are ever classified.
            if let Some(l) = lang {
                if !arg.starts_with('-') || arg == "-" {
                    lang_overrides.push(format!("--c17-x={l}:{arg}"));
                }
            }
            result.push(arg.clone());
            i += 1;
        }
    }

    // A configure probe passes a `-W` or `-f` option to learn whether the
    // compiler takes it, so one c17 does not know fails the run as gcc's
    // would.
    let refused = if driver_errors.is_empty() {
        werror_errors
    } else {
        driver_errors
    };
    if !refused.is_empty() {
        for line in refused {
            eprintln!("c17: {line}");
        }
        std::process::exit(1);
    }

    // Options gathered over the whole scan, placed ahead of any `--`, after
    // which everything is an operand.
    let mut trailer = lang_overrides;
    trailer.append(&mut ignored);
    trailer.extend(
        unsupported
            .into_iter()
            .map(|(_, arg)| format!("--c17-unsupported={arg}")),
    );
    if stack_clash {
        trailer.push("--c17-stack-clash".to_string());
    }
    if debug == Some(true) {
        trailer.push("-g".to_string());
    }
    if signaling_nans {
        trailer.push("-D".to_string());
        trailer.push("__SUPPORT_SNAN__".to_string());
    }
    let at = result
        .iter()
        .position(|a| a == "--")
        .unwrap_or(result.len());
    result.splice(at..at, trailer);
    result
}

/// Answer one of gcc's driver queries, returning the exit status, or `None`
/// when `arg` is not one.
///
/// Build systems run these alone and use the answer verbatim -- in a `-D`
/// macro, a library search path, a cross-compile check -- so each prints
/// exactly the answer and nothing else. The target ones honour `--target`
/// wherever it stands on the line; the link ones go to the host driver, which
/// does the linking. `-dumpversion` is the major version alone, as since gcc
/// 7. (`-v` with no operands is in `compile_main`, since only clap knows
/// what an operand is.)
fn answer_driver_query(rest: &[String], raw_args: &[String]) -> Option<i32> {
    let arg = rest[0].as_str();
    // The two-dash spellings of the queries that take a value may also take
    // it as the next argument, as binutils' configure gives it.
    let joined;
    let arg = if matches!(arg, "--print-prog-name" | "--print-file-name") {
        let Some(value) = rest.get(1) else {
            eprintln!("c17: {} '{arg}'", gettext("error: missing argument to"));
            return Some(1);
        };
        joined = format!("{arg}={value}");
        joined.as_str()
    } else {
        arg
    };
    // gcc takes every `-print-` query with two dashes as well.
    let query = arg
        .strip_prefix('-')
        .filter(|q| q.starts_with("-print-"))
        .unwrap_or(arg);
    match query {
        "-dumpmachine" => println!("{}", query_target(raw_args).gcc_triple()),
        "-print-multiarch" => {
            if let Some(tuple) = query_target(raw_args).multiarch() {
                println!("{tuple}");
            }
        }
        "-dumpversion" => println!("{}", token::preprocess::GNUC_VERSION[0]),
        "-dumpfullversion" => println!("{}", token::preprocess::GNUC_VERSION.join(".")),
        "-print-search-dirs" | "-print-libgcc-file-name" | "-print-multi-os-directory" => {
            return Some(forward_to_host_driver(query));
        }
        _ if query.starts_with("-print-file-name=") => {
            return Some(forward_to_host_driver(query));
        }
        // c17 runs no programs of its own that a build could ask after, so
        // every name is answered as gcc answers one it has no path for: with
        // the name itself, meaning whatever the search path finds.
        _ if query.starts_with("-print-prog-name=") => {
            println!("{}", &query["-print-prog-name=".len()..]);
        }
        _ => return None,
    }
    Some(0)
}

/// The target named by `--target` in either spelling, last one winning, or
/// the host. An unknown triple ends the run, as it would a compile.
fn query_target(raw_args: &[String]) -> Target {
    let mut triple = None;
    let mut it = raw_args.iter().skip(1);
    while let Some(arg) = it.next() {
        if let Some(t) = arg.strip_prefix("--target=") {
            triple = Some(t);
        } else if arg == "--target" {
            triple = it.next().map(String::as_str);
        }
    }
    let Some(triple) = triple else {
        return Target::host();
    };
    Target::from_triple(triple).unwrap_or_else(|| {
        eprintln!("c17: {}: {}", gettext("unsupported target"), triple);
        std::process::exit(1);
    })
}

/// Put `query` to the host driver and pass on its answer and exit status.
fn forward_to_host_driver(query: &str) -> i32 {
    match linkargs::host_driver().arg(query).output() {
        Ok(out) => {
            let _ = io::stdout().write_all(&out.stdout);
            let _ = io::stderr().write_all(&out.stderr);
            out.status.code().unwrap_or(1)
        }
        Err(e) => {
            eprintln!("c17: cc: {e}");
            1
        }
    }
}

/// Parse the rewritten command line, refusing it as gcc's driver would.
///
/// clap's own refusal named no program, followed it with a usage block, and
/// named only the letter it stopped at: configure's `-qversion` probe logged
/// `error: unexpected argument '-q' found`. Build logs are read by people
/// looking for which program said what, so every refusal carries `c17:`, and
/// the two gcc has words for -- an unknown option, a missing value -- use
/// them. The status is gcc's 1, not clap's 2.
fn parse_args(argv: Vec<String>) -> Args {
    use clap::error::ErrorKind;
    match Args::try_parse_from(&argv) {
        Ok(args) => args,
        Err(e) => match e.kind() {
            ErrorKind::DisplayHelp
            | ErrorKind::DisplayVersion
            | ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => e.exit(),
            _ => {
                eprintln!("c17: {}", parse_error_text(&e, &argv));
                std::process::exit(1);
            }
        },
    }
}

/// The text of a parse refusal, without the `c17: ` it is printed after.
fn parse_error_text(e: &clap::Error, argv: &[String]) -> String {
    use clap::error::{ContextKind, ContextValue, ErrorKind};
    let context = |kind| match e.get(kind) {
        Some(ContextValue::String(s)) => Some(s.as_str()),
        _ => None,
    };
    let invalid_arg = context(ContextKind::InvalidArg).unwrap_or_default();
    match e.kind() {
        ErrorKind::UnknownArgument => format!(
            "{} '{}'",
            gettext("error: unrecognized command-line option"),
            unknown_option_culprit(argv, invalid_arg)
        ),
        // clap names the option with its value placeholder, `-o <file>`.
        ErrorKind::InvalidValue if context(ContextKind::InvalidValue) == Some("") => format!(
            "{} '{}'",
            gettext("error: missing argument to"),
            invalid_arg.split(' ').next().unwrap_or_default()
        ),
        // The operands are the only required argument.
        ErrorKind::MissingRequiredArgument => gettext("fatal error: no input files"),
        // clap's first paragraph is the message; the rest is usage and help.
        _ => {
            let text = e.render().to_string();
            let message = text.split("\n\n").next().unwrap_or_default();
            message.trim_end().to_string()
        }
    }
}

/// The argument that held the unknown option clap stopped at.
///
/// clap reads `-qversion` as the short options `-q -v -e ...` and reports the
/// first letter it does not know, so `-q`, where gcc names the whole
/// argument. The argument is the first one that, parsed alone, fails on that
/// same letter: a value such as `-I/q` that merely contains it parses.
fn unknown_option_culprit<'a>(argv: &'a [String], reported: &'a str) -> &'a str {
    use clap::error::{ContextKind, ContextValue, ErrorKind};
    if reported.starts_with("--") || reported.len() != 2 {
        return reported;
    }
    let fails_on_reported = |arg: &str| {
        let Err(e) = Args::try_parse_from([argv[0].as_str(), arg]) else {
            return false;
        };
        e.kind() == ErrorKind::UnknownArgument
            && matches!(e.get(ContextKind::InvalidArg),
                Some(ContextValue::String(s)) if s == reported)
    };
    argv.iter()
        .skip(1)
        .take_while(|a| *a != "--")
        .filter(|a| a.starts_with('-') && !a.starts_with("--") && a.len() > 1)
        .find(|a| *a == reported || fails_on_reported(a))
        .map_or(reported, String::as_str)
}

/// gcc's `-v` banner, printed when `-v` is given with nothing to compile.
///
/// libtool and autoconf run `$CC -v` and log what it says, and probes that
/// want to know which compiler this is look for the line `gcc version`, so
/// that line carries the version `__GNUC__` claims, with c17 in the place
/// gcc puts its package version. `Target:` and `Thread model:` are spelled
/// as gcc spells them.
fn version_banner(target: &Target) -> String {
    format!(
        "c17 version {pkg}\nTarget: {triple}\nThread model: posix\ngcc version {gnuc} (c17 {pkg})\n",
        pkg = env!("CARGO_PKG_VERSION"),
        triple = target.gcc_triple(),
        gnuc = token::preprocess::GNUC_VERSION.join("."),
    )
}

/// Refuse the `-m` flags that ask for code c17 does not generate.
///
/// What stays is what changes nothing: the target's own word size, and
/// choosing or tuning for a CPU, since c17 emits only the architecture's
/// baseline instructions and baseline code runs on every CPU of it. A CPU
/// choice does not define the feature macros (`__AVX2__`, ...) it would in
/// gcc, so code that tests them takes its baseline path. On x86-64 `-msse`,
/// `-msse2` and `-mfpmath=sse` name the baseline itself. Everything else --
/// an instruction-set extension, an ABI or code-model change -- would make
/// the code different from what was asked, so it is an error.
fn check_machine_flags(flags: &[String], target: &Target) {
    let accepted = |flag: &str| {
        flag.starts_with("-march=")
            || flag.starts_with("-mtune=")
            || flag.starts_with("-mcpu=")
            || match target.arch {
                target::Arch::X86_64 => {
                    target::is_isa_flag(flag)
                        || matches!(
                            flag,
                            "-m64"
                                | "-msse"
                                | "-msse2"
                                | "-mmmx"
                                | "-mfpmath=sse"
                                | "-mcmodel=small"
                        )
                }
                target::Arch::Aarch64 => {
                    matches!(flag, "-mabi=lp64" | "-mlittle-endian" | "-mcmodel=small")
                }
            }
            // Both are no-ops. Every function's prologue sets up the frame
            // pointer, leaf or not -- `emit_prologue` in `arch/*/frame.rs`
            // pushes %rbp, or stores x29 and x30, unconditionally -- so the
            // `-mno-` form asks for what is already so, and the other only
            // permits an omission c17 never makes.
            || matches!(
                flag,
                "-mno-omit-leaf-frame-pointer" | "-momit-leaf-frame-pointer"
            )
    };
    let refused: Vec<&String> = flags.iter().filter(|f| !accepted(f)).collect();
    if refused.is_empty() {
        return;
    }
    for flag in refused {
        eprintln!("c17: {}: {}", gettext("unsupported machine flag"), flag);
    }
    std::process::exit(1);
}

/// The language a `-x` name selects: `Some(None)` for `none`, which goes back
/// to reading operands by suffix; `None` for a language c17 does not compile.
fn source_language(name: &str) -> Option<Option<&'static str>> {
    match name {
        "c" => Some(Some("c")),
        "cpp-output" => Some(Some("cpp-output")),
        "assembler" => Some(Some("assembler")),
        "assembler-with-cpp" => Some(Some("assembler-with-cpp")),
        "none" => Some(None),
        _ => None,
    }
}

/// What a `-g` option does to debug information: `Some(Some(on))` for one
/// that turns it on or off, `Some(None)` for none of c17's business, `None`
/// when `arg` is not one of these.
///
/// Every level from 1 up, `-ggdb` and `-gdwarf[-N]` are the DWARF c17 emits;
/// level 0 is none. The level's detail (macro definitions at 3, line tables
/// only at 1) is not something c17 varies.
fn debug_flag_level(arg: &str) -> Option<Option<bool>> {
    let level = match arg {
        "-g" | "-ggdb" | "-gdwarf" => return Some(Some(true)),
        _ => arg
            .strip_prefix("-ggdb")
            .or_else(|| arg.strip_prefix("-gdwarf-"))
            .or_else(|| arg.strip_prefix("-g"))?,
    };
    match level {
        "0" if !arg.starts_with("-gdwarf") => Some(Some(false)),
        "1" | "2" | "3" => Some(Some(true)),
        "4" | "5" if arg.starts_with("-gdwarf-") => Some(Some(true)),
        _ => None,
    }
}

/// `-g` options that tune how debug information is laid out rather than
/// whether there is any.
fn is_known_ignorable_g_flag(arg: &str) -> bool {
    matches!(
        arg,
        "-gsplit-dwarf"
            | "-gz"
            | "-grecord-gcc-switches"
            | "-gno-record-gcc-switches"
            | "-gstrict-dwarf"
            | "-gno-strict-dwarf"
            | "-gcolumn-info"
            | "-gno-column-info"
            | "-gline-tables-only"
            | "-gpubnames"
            | "-gno-pubnames"
    ) || arg.starts_with("-gz=")
}

/// Check if a file is a C source file (by extension)
fn is_source_file(path: &str) -> bool {
    path.ends_with(".c") || path.ends_with(".i") || path == "-"
}

/// A `.i` operand is already the output of `c17 -E` (POSIX 87981).
///
/// Standard input is not one of these: like GCC, c17 reads `-` as ordinary
/// C source, there being no suffix to say otherwise.
fn is_preprocessed_file(path: &str) -> bool {
    path.ends_with(".i")
}

/// Suffixes gcc hands to a front end c17 does not have, each with what c17
/// says about it. gcc fails on these when that front end is not installed
/// ("cannot execute 'cc1plus'"), so c17 fails too, rather than passing a
/// source file to the linker. A C++ header (`.hpp`, ...) is C++ here: gcc
/// precompiles it with the C++ front end.
const FOREIGN_SUFFIXES: &[(&str, &[&str])] = &[
    (
        "c17 does not compile C++",
        &[
            "cc", "cp", "cxx", "cpp", "CPP", "c++", "C", "ii", "hh", "H", "hp", "hxx", "hpp",
            "HPP", "h++", "tcc",
        ],
    ),
    ("c17 does not compile Objective-C", &["m", "mi"]),
    ("c17 does not compile Objective-C++", &["mm", "M", "mii"]),
    (
        "c17 does not compile Fortran",
        &[
            "f", "for", "ftn", "F", "FOR", "FTN", "fpp", "FPP", "f90", "f95", "f03", "f08", "F90",
            "F95", "F03", "F08",
        ],
    ),
    ("c17 does not compile Go", &["go"]),
    ("c17 does not compile D", &["d", "di", "dd"]),
    ("c17 does not compile Ada", &["ads", "adb"]),
    ("c17 does not compile Modula-2", &["mod"]),
];

/// What an operand's suffix says it is, as gcc reads suffixes. One that names
/// no language is a linker input -- an object or library under any name, a
/// linker script, a version script -- which gcc passes to the linker as-is.
fn lang_by_suffix(path: &str) -> Lang {
    if is_preprocessed_file(path) {
        return Lang::Preprocessed;
    }
    if is_source_file(path) {
        return Lang::C;
    }
    let suffix = Path::new(path).extension().and_then(|s| s.to_str());
    match suffix {
        Some("S" | "sx") => Lang::AsmCpp,
        Some("s") => Lang::Asm,
        Some("h") => Lang::Header,
        Some(suffix) => FOREIGN_SUFFIXES
            .iter()
            .find(|(_, suffixes)| suffixes.contains(&suffix))
            .map_or(Lang::LinkerInput, |(why, _)| Lang::Foreign(why)),
        None => Lang::LinkerInput,
    }
}

/// What kind of thing a pathname operand names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperandKind {
    /// `.c`, `.i`, or a bare `-`; or a `.h` under `-E`, `-M` or `-MM`.
    Source,
    /// `.s`, `.S` or `.sx`.
    Asm,
    /// Anything the linker reads: an object or library, a linker script, or
    /// whatever else names no language. It goes to the linker in its place.
    LinkerInput,
    /// A language c17 does not compile, with what to say about it.
    Foreign(&'static str),
}

/// A pathname operand together with its kind, keeping argument order.
#[derive(Debug, Clone)]
struct Operand {
    path: String,
    kind: OperandKind,
}

/// What an operand is read as: by `-x` when one applied to it, else by its
/// suffix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lang {
    /// C source: `.c`, `-`, or `-x c`.
    C,
    /// Already preprocessed: `.i`, or `-x cpp-output`.
    Preprocessed,
    /// Assembly: `.s`, or `-x assembler`.
    Asm,
    /// Assembly to preprocess first: `.S`, `.sx`, or
    /// `-x assembler-with-cpp`.
    AsmCpp,
    /// A C header, `.h`: gcc preprocesses it under `-E`, `-M` and `-MM`, and
    /// otherwise writes a precompiled header.
    Header,
    /// A suffix that names no language: for the linker.
    LinkerInput,
    /// A language gcc compiles and c17 does not, with what to say about it.
    Foreign(&'static str),
}

/// Whether a linker-input operand never reaches the linker in this run, so
/// that the driver itself must check it exists.
///
/// gcc errors "linker input file not found" for a missing linker input when
/// nothing is linked (`-c`, `-S`, `-E`); a build naming a file it never made
/// must fail, not succeed with a warning. When linking, the linker reports a
/// missing input itself.
fn bypasses_linker(kind: OperandKind, link_phase: bool) -> bool {
    match kind {
        OperandKind::LinkerInput => !link_phase,
        OperandKind::Source | OperandKind::Asm | OperandKind::Foreign(_) => false,
    }
}

impl Operand {
    fn classify(path: String, args: &Args) -> Self {
        let kind = match args.lang_of(&path) {
            Lang::C | Lang::Preprocessed => OperandKind::Source,
            Lang::Header if args.preprocess_only || args.dependencies_replace_output() => {
                OperandKind::Source
            }
            Lang::Header => OperandKind::Foreign("c17 does not write precompiled headers"),
            Lang::Asm | Lang::AsmCpp => OperandKind::Asm,
            Lang::LinkerInput => OperandKind::LinkerInput,
            Lang::Foreign(why) => OperandKind::Foreign(why),
        };
        Operand { path, kind }
    }
}

/// Name a scratch file inside the per-run temporary directory.
///
/// The directory itself is created by `tempfile`, which is why these names
/// need no PID or randomness of their own: the directory is unpredictable,
/// created with `O_EXCL`, and removed when the run ends.
///
/// The name is prefixed with the operand's position because one run compiles
/// every operand into this one directory, and a file stem is not unique across
/// them: `c17 a/util.c b/util.c` names both outputs `util.o`.
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
    let needs_cpp = args.lang_of(path) == Lang::AsmCpp;
    let asm_to_assemble = if needs_cpp {
        let temp_s = scratch_path(scratch, operand_id, stem, "s");
        // A BOM is stripped here for the same reason it is on every other
        // reader: translation phase 1 has no byte for it, and `as` reads the
        // leading 0xEF as the first character of a mnemonic. `-E` on the same
        // file already stripped it, so without this a BOM'd `.S` preprocessed
        // clean and failed to assemble. Only `.S` gets this -- a `.s` is handed
        // to `as` untouched, which is what gcc does with it too.
        let content = strip_bom(&std::fs::read(path)?).to_vec();
        let asm_config = AsmPreprocessConfig {
            optimization: args.optimization(),
            position: position_independence(args, target),
            isa: target::X86Isa::from_flags(&args.mflags),
            defines: &args.defines,
            undefines: &args.undefines,
            include_paths: &args.include_paths,
            search: system_search(args),
            no_std_inc: args.no_std_inc,
            macro_prefix_map: args.prefix_maps().macros,
        };
        // Catches #error, a missing include, and friends.
        let preprocessed =
            preprocess_asm_file(&content, target, path, &asm_config).map_err(|e| {
                diag::reset_counts();
                io::Error::other(e.to_string())
            })?;
        std::fs::write(&temp_s, &preprocessed)?;
        temp_s
    } else {
        path.to_string()
    };

    let status = AssemblerCommand::new(
        target.os,
        args.debug > 0,
        &args.prefix_maps().debug,
        &asm_to_assemble,
        &obj_file,
    )
    .command()
    .status()?;

    if needs_cpp {
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

/// How to assemble one `.s` file: the program and its arguments.
///
/// Normally this is the system `as`. On Darwin with a debug prefix map in
/// effect it is the host driver instead: Apple's assembler is clang's
/// integrated assembler, which records its own working directory in the DWARF
/// line table unless it is told the mapping, and only the driver forwards
/// `-fdebug-prefix-map` to it. GNU `as` records no such directory.
#[derive(Debug, Clone, PartialEq, Eq)]
struct AssemblerCommand {
    program: &'static str,
    args: Vec<String>,
}

impl AssemblerCommand {
    fn new(os: Os, debug: bool, debug_map: &PrefixMap, input: &str, output: &str) -> Self {
        let mut args = Vec::new();
        let program = if os == Os::MacOS && !debug_map.is_empty() {
            args.extend(["-c", "-x", "assembler", input, "-o", output].map(String::from));
            if debug {
                args.push("-g".to_string());
            }
            args.extend(
                debug_map
                    .entries()
                    .map(|(old, new)| format!("-fdebug-prefix-map={old}={new}")),
            );
            linkargs::HOST_DRIVER
        } else {
            if debug {
                args.push("-g".to_string());
            }
            args.extend(["-o", output, input].map(String::from));
            "as"
        };
        AssemblerCommand { program, args }
    }

    fn command(&self) -> Command {
        let mut cmd = Command::new(self.program);
        cmd.args(&self.args);
        cmd
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
        out.extend(args.linker_flags.iter().cloned().map(LinkItem::Flag));
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
            linkargs::LinkArg::Flag(f) => out.push(LinkItem::Flag(f.clone())),
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

/// The position independence this compilation generates code with, and its
/// `__PIC__`/`__PIE__` macros describe: the last of the `-fpic` family, or
/// the target's default. `-shared` is a link option and changes neither, as
/// in gcc; the code generator makes shared-object code of its own accord
/// (see `process_file`).
fn position_independence(args: &Args, target: &Target) -> target::PositionIndependence {
    args.pic_flag
        .unwrap_or_else(|| target::PositionIndependence::target_default(target))
}

/// The stack the compiler runs on.
///
/// The front end descends recursively through the source: about fifteen Rust
/// frames for every `(` of an expression, one per nested `struct`, and one per
/// `case` label, since a labeled statement holds the statement it labels. C17
/// 5.2.4.1 asks for 63 levels of parenthesised expression and 63 of nested
/// structure, and real generated source goes far past that -- the torture
/// suite alone has a `switch` with a thousand consecutive labels.
///
/// The default 8 MB is not enough for those, and running out of it is a Rust
/// panic about a stack overflow rather than a diagnostic naming a translation
/// limit. Running the compile on a thread of our own makes the size ours to
/// choose rather than the shell's.
const COMPILER_STACK_BYTES: usize = 256 * 1024 * 1024;

fn main() -> ! {
    // `RUST_MIN_STACK` is honoured, so a build that needs still more has a way
    // to say so without a rebuild.
    let stack = std::env::var("RUST_MIN_STACK")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(COMPILER_STACK_BYTES);
    // The error is reported on the compiler thread and turned into a status
    // there: `Box<dyn Error>` is not `Send`, and there is nothing useful to
    // carry back across the join anyway.
    std::thread::Builder::new()
        .stack_size(stack)
        .spawn(|| match compile_main() {
            Ok(()) => 0,
            Err(e) => {
                eprintln!("c17: {e}");
                1
            }
        })
        .expect("failed to start the compiler thread")
        .join()
        .map(std::process::exit)
        .unwrap_or_else(|_| std::process::exit(1))
}

fn compile_main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("c17");

    let argv = preprocess_args();
    // Rescan for the -L/-l/-R order relative to the operands, which clap's
    // per-flag collection cannot preserve. Done before parsing so a parse
    // failure still exits the usual way.
    let scanned = linkargs::scan(argv.iter().cloned());
    let args = parse_args(argv);

    if args.no_warnings {
        diag::suppress_warnings();
    }
    if args.fpermissive {
        diag::set_permissive();
    }
    // Both of these were parsed into fields nothing read, so the flags were
    // accepted and did nothing. They now mean what they mean to gcc: the
    // builtins whose names do not begin with `__builtin_` stop being builtins.
    if args.fno_builtin {
        builtins::set_no_builtin();
    }
    // `-fno-gnu89-inline` is the default, and `overrides_with` makes the last
    // of the pair on the command line the one that survives.
    if args.fgnu89_inline {
        builtins::set_gnu89_inline(true);
    }
    if !args.fno_builtin_funcs.is_empty() {
        builtins::set_no_builtin_funcs(args.fno_builtin_funcs.iter().cloned().collect());
    }
    // The `-W` options reach the places that emit warnings, which are
    // nowhere near here: the groups `-Wno-<name>` turns off, `-Wpedantic`
    // and `-pedantic-errors`, and `-Werror` with its relatives.
    let warning_options: Vec<&str> = args.warnings.iter().map(String::as_str).collect();
    diag::set_warning_options(&warning_options);

    // Set when `-Werror` makes an error of a command-line warning; see
    // `driver_leniency`. Every such warning is given before stopping.
    let mut refused = false;
    for option in &args.ignored_options {
        refused |= driver_leniency(&format!(
            "{}: {}",
            gettext("unrecognized option, ignored"),
            option
        ));
    }
    for option in &args.unsupported_options {
        refused |= diag::command_line_group_warning(
            f_options::UNSUPPORTED_WARNING,
            &gettext_args("'{0}' is not supported; ignored", &[option]),
        );
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
        // Say plainly that C90 was not honoured.
        // c17 compiles C17 and only C17, so the flag is accepted -- build
        // systems pass it unconditionally -- but silently ignoring it is what
        // let __STDC_VERSION__ disagree with the binary's own name once
        // already. A C99 or C11 program is a C17 program, so asking for
        // either is met.
        Ok(Some(StdRequest::Ignored)) if diag::warning_group_enabled(STD_DIALECT_WARNING) => {
            let spec = args.c17_std.as_deref().unwrap_or_default();
            driver_warning(&gettext_args(
                "'-std={0}' ignored; c17 compiles C17 (ISO/IEC 9899:2018) only",
                &[spec],
            ));
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
    let mut target = if let Some(ref triple) = args.target {
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

    if args.verbose && args.files.is_empty() {
        eprint!("{}", version_banner(&target));
        return Ok(());
    }

    check_machine_flags(&args.mflags, &target);
    if target.arch == target::Arch::X86_64 {
        target.x86_isa = target::X86Isa::from_flags(&args.mflags);
    }
    // Before anything reads it: the type system, the predefined macros and
    // the preprocessor's character constants all take it from the target.
    if let Some(signedness) = args.plain_char {
        target.plain_char = signedness;
    }

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
        .map(|f| Operand::classify(f.clone(), &args))
        .collect();

    let source_count = operands
        .iter()
        .filter(|o| o.kind == OperandKind::Source)
        .count();

    // A single -o names one output. With -c and several sources it would name
    // each of them in turn, so every object but the last is overwritten. The
    // spec leaves this unspecified (88338-88343); say so rather than silently
    // producing one object.
    if args.compile_only && args.output.is_some() && source_count > 1 {
        refused |= driver_leniency(&format!(
            "{} ({})",
            gettext("-o applies only to the last source operand with -c"),
            source_count
        ));
    }

    // `-M`/`-MM` is the one combination that cannot be reduced to a warning.
    // The other two leave *something* usable behind -- the last object, or the
    // concatenated `-E` text -- but a dependency file is read by make, and a
    // `.d` holding only the last source's rule is silently wrong: make sees no
    // prerequisites for the others and stops rebuilding them. There is no
    // partial answer worth writing, so refuse instead. `-MF` names the same
    // single file for the same reason, and `-o` also stands in for it here
    // (`dependency_sink`), so both spellings are caught.
    let deps_to_one_file =
        args.deps_file.is_some() || args.output.as_deref().is_some_and(|p| p != "-");
    if args.dependencies_replace_output() && deps_to_one_file && source_count > 1 {
        eprintln!(
            "c17: {} ({})",
            gettext("cannot write the dependency rules for several sources to one file"),
            source_count
        );
        std::process::exit(1);
    }

    // -E is the other unspecified combination, and it resolves the other way:
    // the operands share one output stream, so they concatenate rather than
    // overwrite. Still worth saying, since a makefile expecting one .i per
    // source gets one file holding all of them.
    if args.preprocess_only && args.output.is_some() && source_count > 1 {
        refused |= driver_leniency(&format!(
            "{} ({})",
            gettext("-o collects every source operand into one file with -E"),
            source_count
        ));
    }
    if refused {
        std::process::exit(1);
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
    let scratch = plib::tmp::Builder::new().prefix("c17-").tempdir()?;

    // Opened once, before the loop, so several source operands concatenate
    // into one `-E` output rather than each truncating the last.
    let mut pp_out = preprocess_sink(&args)?;
    // The object each operand contributes to the link, by operand index.
    // `None` means the operand contributes nothing (`-c`, an early-exit mode,
    // or a language c17 does not compile).
    let mut operand_objects: Vec<Option<String>> = vec![None; operands.len()];
    // CONSEQUENCES OF ERRORS (88185-88187): diagnose, keep compiling the
    // remaining operands, skip the link, exit non-zero.
    let mut failed = false;

    let link_phase = !args.compile_only
        && !args.asm_only
        && !args.preprocess_only
        && !args.dependencies_replace_output()
        && !args.dump_tokens
        && !args.dump_ast
        && args.dump_ir.is_none();

    for (idx, op) in operands.iter().enumerate() {
        if op.kind == OperandKind::LinkerInput && !link_phase {
            unused_linker_input(&op.path);
        }
        if bypasses_linker(op.kind, link_phase) {
            if let Err(e) = std::fs::metadata(&op.path) {
                eprintln!(
                    "c17: {}: {}: {}: {}",
                    gettext("error"),
                    op.path,
                    gettext("linker input file not found"),
                    plib::diag::io_error_text(&e)
                );
                failed = true;
                continue;
            }
        }
        match op.kind {
            OperandKind::Foreign(why) => {
                eprintln!("c17: {}: {}: {}", gettext("error"), op.path, gettext(why));
                failed = true;
            }
            OperandKind::LinkerInput => operand_objects[idx] = Some(op.path.clone()),
            // 87883-87885: with -E "no compilation shall be performed", so an
            // assembler operand is never handed to `as`. Not assembling is not
            // the same as producing nothing, though: preprocess it and write
            // the text, as gcc does.
            OperandKind::Asm if args.preprocess_only => {
                let result = preprocess_asm_operand(&op.path, &args, &target, &mut pp_out);
                diag::finish_unit();
                match result {
                    Ok(()) => {}
                    Err(e) => {
                        eprintln!("c17: {}: {}", op.path, plib::diag::io_error_text(&e));
                        failed = true;
                    }
                }
            }
            OperandKind::Asm => {
                let result = assemble_operand(&op.path, &args, &target, scratch.path(), idx);
                diag::finish_unit();
                match result {
                    Ok(Some(obj)) => operand_objects[idx] = Some(obj),
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("c17: {}: {}", op.path, plib::diag::io_error_text(&e));
                        failed = true;
                    }
                }
            }
            OperandKind::Source => {
                let obj_name = source_object_name(&op.path, &args, scratch.path(), idx);
                let mut outputs = Outputs {
                    object: &obj_name,
                    preprocessed: &mut pp_out,
                };
                let result =
                    process_file(&op.path, &args, &target, &mut outputs, scratch.path(), idx);
                // gcc's "warnings being treated as errors" closes the unit's
                // own diagnostics, ahead of the driver's verdict on it.
                diag::finish_unit();
                match result {
                    Ok(Compiled::Nothing) => {}
                    Ok(Compiled::Object { path, temporary }) => {
                        if temporary {
                            operand_objects[idx] = Some(path);
                        }
                    }
                    Err(e) => {
                        eprintln!("c17: {}: {}", op.path, plib::diag::io_error_text(&e));
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

    let has_object = link_line.iter().any(|i| matches!(i, LinkItem::Object(_)));

    if !failed && link_phase && has_object {
        let exe_file = args.output.clone().unwrap_or_else(|| "a.out".to_string());
        if let Err(e) = link_objects(&link_line, &exe_file, &args, &target) {
            eprintln!("c17: {}", plib::diag::io_error_text(&e));
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

    /// `-s` reaches GNU ld as a flag; on macOS, whose ld64 ignores it, the
    /// output is run through `strip` instead.
    #[test]
    fn test_strip_by_os() {
        assert_eq!(StripBy::for_os(Os::Linux), StripBy::LinkerFlag);
        assert_eq!(StripBy::for_os(Os::FreeBSD), StripBy::LinkerFlag);
        assert_eq!(StripBy::for_os(Os::MacOS), StripBy::StripTool);
    }

    /// Only a linker input the linker will not see is checked by the driver:
    /// one named when nothing is linked.
    #[test]
    fn test_bypasses_linker() {
        for link_phase in [false, true] {
            assert!(!bypasses_linker(OperandKind::Source, link_phase));
            assert!(!bypasses_linker(OperandKind::Asm, link_phase));
            assert!(!bypasses_linker(OperandKind::Foreign("x"), link_phase));
        }
        assert!(bypasses_linker(OperandKind::LinkerInput, false));
        assert!(!bypasses_linker(OperandKind::LinkerInput, true));
    }

    /// Objects and libraries under their usual names, and everything else
    /// that names no language, are for the linker.
    #[test]
    fn test_lang_by_suffix_linker_input() {
        for path in [
            "foo.o",
            "/path/to/bar.o",
            "libfoo.a",
            "libfoo.so",
            "libfoo.dylib",
            "libz.so.1.3.1",
            "/usr/lib/libssl.so.3",
            "f.weird",
            "extra.ld",
            "exports.map",
            "baz.txt",
            "t.def",
            "noext",
            "dir.d/noext",
        ] {
            assert_eq!(lang_by_suffix(path), Lang::LinkerInput, "{path}");
        }
    }

    #[test]
    fn test_lang_by_suffix_languages() {
        assert_eq!(lang_by_suffix("a.c"), Lang::C);
        assert_eq!(lang_by_suffix("-"), Lang::C);
        assert_eq!(lang_by_suffix("a.i"), Lang::Preprocessed);
        assert_eq!(lang_by_suffix("a.s"), Lang::Asm);
        assert_eq!(lang_by_suffix("a.S"), Lang::AsmCpp);
        assert_eq!(lang_by_suffix("a.sx"), Lang::AsmCpp);
        assert_eq!(lang_by_suffix("a.h"), Lang::Header);
    }

    /// Every suffix gcc gives another front end is refused, naming it.
    #[test]
    fn test_lang_by_suffix_foreign() {
        for (path, why) in [
            ("a.cpp", "c17 does not compile C++"),
            ("a.cc", "c17 does not compile C++"),
            ("a.c++", "c17 does not compile C++"),
            ("a.C", "c17 does not compile C++"),
            ("a.ii", "c17 does not compile C++"),
            ("a.hpp", "c17 does not compile C++"),
            ("a.m", "c17 does not compile Objective-C"),
            ("a.mm", "c17 does not compile Objective-C++"),
            ("a.f90", "c17 does not compile Fortran"),
            ("a.F", "c17 does not compile Fortran"),
            ("a.go", "c17 does not compile Go"),
            ("a.d", "c17 does not compile D"),
            ("a.adb", "c17 does not compile Ada"),
            ("a.mod", "c17 does not compile Modula-2"),
        ] {
            assert_eq!(lang_by_suffix(path), Lang::Foreign(why), "{path}");
        }
    }

    // Tests for is_source_file()

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

    // Tests for preprocess_args()

    fn run_preprocess(args: &[&str]) -> Vec<String> {
        let raw_args: Vec<String> = std::iter::once("c17".to_string())
            .chain(args.iter().map(|s| s.to_string()))
            .collect();
        preprocess_args_from(raw_args)
    }

    #[test]
    fn test_preprocess_std_is_forwarded_not_dropped() {
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
            Ok(Some(StdRequest::Compiled))
        );
        // A C99 program is a C17 program.
        assert_eq!(
            parse(&["-std=c99", "foo.c"]).std_request(),
            Ok(Some(StdRequest::Compiled))
        );
        // Recognized but not C17's: accepted, and reported as not honoured.
        assert_eq!(
            parse(&["-std=c90", "foo.c"]).std_request(),
            Ok(Some(StdRequest::Ignored))
        );
        // A typo is still an error, not a silently ignored value.
        assert_eq!(parse(&["-std=c42", "foo.c"]).std_request(), Err("c42"));
        // So is a revision after C17, which c17 cannot compile.
        assert_eq!(parse(&["-std=c23", "foo.c"]).std_request(), Err("c23"));
    }

    #[test]
    fn test_preprocess_plain_char_spellings() {
        for (flag, want) in [
            ("-funsigned-char", "unsigned"),
            ("-fno-signed-char", "unsigned"),
            ("-fsigned-char", "signed"),
            ("-fno-unsigned-char", "signed"),
        ] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(
                result.contains(&format!("--c17-plain-char={want}")),
                "{flag}: {result:?}"
            );
            assert!(!result.contains(&flag.to_string()), "{flag}");
        }
    }

    #[test]
    fn test_plain_char_last_flag_wins() {
        use target::CharSignedness::{Signed, Unsigned};
        let parse = |argv: &[&str]| Args::parse_from(run_preprocess(argv)).plain_char;
        assert_eq!(parse(&["foo.c"]), None);
        assert_eq!(parse(&["-funsigned-char", "foo.c"]), Some(Unsigned));
        assert_eq!(parse(&["-fno-unsigned-char", "foo.c"]), Some(Signed));
        assert_eq!(
            parse(&["-fsigned-char", "-funsigned-char", "foo.c"]),
            Some(Unsigned)
        );
        assert_eq!(
            parse(&["-funsigned-char", "-fsigned-char", "foo.c"]),
            Some(Signed)
        );
        assert_eq!(
            parse(&["-funsigned-char", "foo.c", "-fno-unsigned-char"]),
            Some(Signed)
        );
        assert_eq!(
            parse(&["-fsigned-char", "-fno-signed-char", "foo.c"]),
            Some(Unsigned)
        );
    }

    /// `-fcf-protection[=level]` and `-fno-cf-protection` are one option,
    /// the last occurrence winning. No flag is `none`, the bare flag is
    /// `full`, and `check` builds what `none` does, as in gcc.
    #[test]
    fn test_cf_protection_last_flag_wins() {
        use target::CfProtection;
        let parse = |argv: &[&str]| {
            let result = run_preprocess(argv);
            assert!(
                !result
                    .iter()
                    .any(|a| a.starts_with("-fcf") || a.starts_with("-fno-cf")),
                "{result:?}"
            );
            Args::parse_from(result).cf_protection.unwrap_or_default()
        };
        let none = CfProtection::default();
        let full = CfProtection {
            branch: true,
            ret: true,
        };
        let branch = CfProtection {
            branch: true,
            ret: false,
        };
        let ret = CfProtection {
            branch: false,
            ret: true,
        };
        assert_eq!(parse(&["foo.c"]), none);
        assert_eq!(parse(&["-fcf-protection", "foo.c"]), full);
        assert_eq!(parse(&["-fcf-protection=full", "foo.c"]), full);
        assert_eq!(parse(&["-fcf-protection=branch", "foo.c"]), branch);
        assert_eq!(parse(&["-fcf-protection=return", "foo.c"]), ret);
        assert_eq!(parse(&["-fcf-protection=none", "foo.c"]), none);
        assert_eq!(parse(&["-fcf-protection=check", "foo.c"]), none);
        assert_eq!(parse(&["-fno-cf-protection", "foo.c"]), none);
        assert_eq!(
            parse(&["-fcf-protection", "-fno-cf-protection", "foo.c"]),
            none
        );
        assert_eq!(
            parse(&["-fno-cf-protection", "foo.c", "-fcf-protection=return"]),
            ret
        );
        assert_eq!(
            parse(&["-fcf-protection=branch", "-fcf-protection=none", "foo.c"]),
            none
        );
        assert_eq!(
            parse(&["-fcf-protection=none", "-fcf-protection=branch", "foo.c"]),
            branch
        );
        assert_eq!(CfProtection::from_level("bogus"), None);
        assert_eq!(CfProtection::from_level(""), None);
        assert_eq!(CfProtection::from_level("Full"), None);
    }

    #[test]
    fn test_prefix_maps_keep_command_line_order() {
        // The three spellings share one list, so a later `-fdebug-prefix-map`
        // overrides an earlier `-ffile-prefix-map` for debug info only.
        let args = Args::parse_from(run_preprocess(&[
            "-ffile-prefix-map=/b=.",
            "foo.c",
            "-fdebug-prefix-map=/b=/D",
            "-fmacro-prefix-map=/a=b=/M",
        ]));
        let maps = args.prefix_maps();
        assert_eq!(maps.debug.apply("/b/t.c"), "/D/t.c");
        assert_eq!(maps.macros.apply("/b/t.c"), "./t.c");
        assert_eq!(maps.macros.apply("/a=b/t.c"), "/M/t.c");
        assert_eq!(maps.debug.apply("/a=b/t.c"), "/a=b/t.c");
        assert_eq!(args.files, ["foo.c"]);
    }

    fn assembler(os: Os, debug: bool, map: &[(&str, &str)]) -> (&'static str, Vec<String>) {
        let mut debug_map = PrefixMap::default();
        for (old, new) in map {
            debug_map.push(old, new);
        }
        let cmd = AssemblerCommand::new(os, debug, &debug_map, "t.s", "t.o");
        (cmd.program, cmd.args)
    }

    #[test]
    fn test_darwin_prefix_map_assembles_with_the_driver() {
        // Apple's integrated assembler records its own cwd in the line table
        // unless the driver forwards the map to it, in command-line order.
        let (program, args) = assembler(Os::MacOS, true, &[("/b", "."), ("/b/x", "/X")]);
        assert_eq!(program, "cc");
        assert_eq!(
            args,
            [
                "-c",
                "-x",
                "assembler",
                "t.s",
                "-o",
                "t.o",
                "-g",
                "-fdebug-prefix-map=/b=.",
                "-fdebug-prefix-map=/b/x=/X",
            ]
        );
        let (program, args) = assembler(Os::MacOS, false, &[("/b", ".")]);
        assert_eq!(program, "cc");
        assert_eq!(
            args,
            [
                "-c",
                "-x",
                "assembler",
                "t.s",
                "-o",
                "t.o",
                "-fdebug-prefix-map=/b=."
            ]
        );
    }

    #[test]
    fn test_assembler_is_plain_as_otherwise() {
        // Darwin without a map, and GNU as with one, are unchanged.
        for (os, map) in [
            (Os::MacOS, &[][..]),
            (Os::Linux, &[("/b", ".")][..]),
            (Os::FreeBSD, &[("/b", ".")][..]),
        ] {
            assert_eq!(
                assembler(os, true, map),
                (
                    "as",
                    vec!["-g".into(), "-o".into(), "t.o".into(), "t.s".into()]
                ),
                "{os:?}"
            );
            assert_eq!(
                assembler(os, false, map),
                ("as", vec!["-o".into(), "t.o".into(), "t.s".into()]),
                "{os:?}"
            );
        }
    }

    #[test]
    fn test_preprocess_fpic_uppercase() {
        let result = run_preprocess(&["-fPIC", "foo.c"]);
        assert!(result.contains(&"--c17-pic=-fPIC".to_string()));
        assert!(!result.contains(&"-fPIC".to_string()));
    }

    #[test]
    fn test_preprocess_fpic_lowercase() {
        let result = run_preprocess(&["-fpic", "foo.c"]);
        assert!(result.contains(&"--c17-pic=-fpic".to_string()));
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
        assert!(result.contains(&"--c17-pic=-fPIC".to_string()));
        assert!(result.contains(&"--shared".to_string()));
        assert!(result.contains(&"-l".to_string()));
        assert!(result.contains(&"z".to_string()));
        assert!(result.contains(&"-L".to_string()));
        assert!(result.contains(&".".to_string()));
    }

    // Tests for silently-ignored flags

    #[test]
    fn test_preprocess_fvisibility_is_kept() {
        let result = run_preprocess(&["-fvisibility=hidden", "foo.c"]);
        assert!(result.contains(&"--c17-visibility=hidden".to_string()));
        assert!(!result.contains(&"-fvisibility=hidden".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    /// An option whose effect c17 does not provide reaches the driver as a
    /// request to warn, the last of its family winning and a later `-fno-`
    /// withdrawing it.
    #[test]
    fn test_preprocess_unsupported_f_flags_are_kept_for_the_warning() {
        for flag in &[
            "-fstack-protector",
            "-fstack-protector-strong",
            "-fstack-protector-all",
            "-fsanitize=address",
            "-fcommon",
        ] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(
                result.contains(&format!("--c17-unsupported={flag}")),
                "{flag}: {result:?}"
            );
            assert!(result.contains(&"foo.c".to_string()));
        }
        let unsupported = |args: &[&str]| -> Vec<String> {
            run_preprocess(args)
                .into_iter()
                .filter(|a| a.starts_with("--c17-unsupported="))
                .collect()
        };
        assert_eq!(
            unsupported(&["-fstack-protector", "-fstack-protector-strong"]),
            ["--c17-unsupported=-fstack-protector-strong"]
        );
        assert!(unsupported(&["-fstack-protector-all", "-fno-stack-protector"]).is_empty());
        assert_eq!(
            unsupported(&["-fno-stack-protector", "-fstack-protector"]),
            ["--c17-unsupported=-fstack-protector"]
        );
    }

    /// What c17's output already is is taken in silence, and leaves nothing
    /// behind.
    #[test]
    fn test_preprocess_accepted_f_flags_leave_nothing() {
        for flag in &[
            "-fno-semantic-interposition",
            "-fno-reorder-blocks-and-partition",
            "-fno-plt",
            "-fno-common",
            "-fdiagnostics-color=always",
            "-flto=auto",
            "-ffat-lto-objects",
        ] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert_eq!(result, ["c17", "foo.c"], "{flag}");
        }
    }

    /// Every option the table says the driver implements is parsed before
    /// the table is consulted: a debug build asserts it.
    #[test]
    fn test_preprocess_implemented_f_flags_are_parsed() {
        for flag in &[
            "-fPIC",
            "-fno-pie",
            "-fcf-protection",
            "-fcf-protection=branch",
            "-fno-cf-protection",
            "-ffile-prefix-map=/a=/b",
            "-fdebug-prefix-map=/a=/b",
            "-fmacro-prefix-map=/a=/b",
            "-fgnu89-inline",
            "-fno-gnu89-inline",
            "-fhosted",
            "-finline",
            "-fno-inline",
            "-fmath-errno",
            "-fno-math-errno",
            "-fno-builtin",
            "-fno-builtin-memcpy",
            "-fpermissive",
            "-fsignaling-nans",
            "-fno-signaling-nans",
            "-fsigned-char",
            "-fno-unsigned-char",
            "-fstack-clash-protection",
            "-fno-stack-clash-protection",
            "-ftls-model=initial-exec",
            "-ftrapping-math",
            "-fno-trapping-math",
            "-fuse-ld=lld",
            "-fverbose-asm",
            "-fvisibility=hidden",
        ] {
            assert_eq!(
                posixutils_cc::f_options::classify(&flag[2..]),
                posixutils_cc::f_options::Verdict::Known(Effect::Implemented),
                "{flag}"
            );
            run_preprocess(&[flag, "foo.c"]);
        }
        let result = run_preprocess(&["-fuse-ld=lld", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-fuse-ld=lld".to_string()));
        assert!(run_preprocess(&["-fstack-clash-protection"]).contains(&"--c17-stack-clash".into()));
        assert!(
            !run_preprocess(&["-fstack-clash-protection", "-fno-stack-clash-protection"])
                .contains(&"--c17-stack-clash".into())
        );
    }

    #[test]
    fn test_preprocess_m_flags_are_kept_for_the_target_check() {
        let result = run_preprocess(&["-msse2", "foo.c"]);
        assert!(result.contains(&"--c17-mflag=-msse2".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    #[test]
    fn test_preprocess_x_applies_to_later_operands() {
        let result = run_preprocess(&[
            "a.h",
            "-x",
            "c",
            "b.h",
            "-xassembler-with-cpp",
            "c.asm",
            "-x",
            "none",
            "d.c",
        ]);
        assert!(!result.iter().any(|a| a.ends_with(":a.h")));
        assert!(result.contains(&"--c17-x=c:b.h".to_string()));
        assert!(result.contains(&"--c17-x=assembler-with-cpp:c.asm".to_string()));
        assert!(!result.iter().any(|a| a.ends_with(":d.c")));
        for operand in ["a.h", "b.h", "c.asm", "d.c"] {
            assert!(result.contains(&operand.to_string()), "{operand}");
        }
    }

    #[test]
    fn test_preprocess_debug_levels_last_wins() {
        for (flags, want) in [
            (&["-g3"][..], true),
            (&["-ggdb"][..], true),
            (&["-gdwarf-4"][..], true),
            (&["-g1", "-g0"][..], false),
            (&["-g0", "-g2"][..], true),
            (&["-gsplit-dwarf"][..], false),
        ] {
            let mut argv = flags.to_vec();
            argv.push("foo.c");
            let result = run_preprocess(&argv);
            assert_eq!(result.contains(&"-g".to_string()), want, "{flags:?}");
            assert!(
                !result.iter().any(|a| a.starts_with("-g") && a != "-g"),
                "{flags:?}"
            );
        }
    }

    #[test]
    fn test_preprocess_ansi_and_pedantic() {
        let result = run_preprocess(&["-ansi", "-pedantic-errors", "-pedantic", "foo.c"]);
        assert!(result
            .windows(2)
            .any(|w| w[0] == "--c17-std" && w[1] == "c90"));
        assert!(result
            .windows(2)
            .any(|w| w[0] == "-W" && w[1] == "pedantic-errors"));
        assert!(result
            .windows(2)
            .any(|w| w[0] == "-W" && w[1] == "pedantic"));
        assert!(!result.iter().any(|a| a.starts_with("-pedantic")));
    }

    #[test]
    fn test_preprocess_pipe_ignored() {
        let result = run_preprocess(&["-pipe", "foo.c"]);
        assert!(!result.contains(&"-pipe".to_string()));
        assert!(result.contains(&"foo.c".to_string()));
    }

    /// The `-fpic` family is one option, the last member winning: every
    /// `-fno-` spelling is position-dependent code, PIE included, and the link
    /// options `-pie`, `-no-pie` and `-static-pie` are not members.
    #[test]
    fn test_pic_family_last_one_wins() {
        use target::{PicLevel, PositionIndependence as P};
        let linux = Target::new(target::Arch::X86_64, Os::Linux);
        let position = |argv: &[&str]| {
            let args = Args::parse_from(run_preprocess(argv));
            position_independence(&args, &linux)
        };
        assert_eq!(position(&["foo.c"]), P::Pie(PicLevel::Large));
        assert_eq!(position(&["-fpic", "foo.c"]), P::Pic(PicLevel::Small));
        assert_eq!(position(&["-fPIE", "foo.c"]), P::Pie(PicLevel::Large));
        for flag in ["-fno-pic", "-fno-PIC", "-fno-pie", "-fno-PIE"] {
            assert_eq!(position(&[flag, "foo.c"]), P::Absolute, "{flag}");
        }
        assert_eq!(position(&["-fPIC", "-fno-pie", "foo.c"]), P::Absolute);
        assert_eq!(
            position(&["-fno-pic", "-fpie", "foo.c"]),
            P::Pie(PicLevel::Small)
        );
        assert_eq!(
            position(&["-fpie", "-fPIC", "foo.c"]),
            P::Pic(PicLevel::Large)
        );
        for link in ["-pie", "-no-pie", "-static-pie", "-shared"] {
            assert_eq!(
                position(&[link, "foo.c"]),
                P::Pie(PicLevel::Large),
                "{link}"
            );
        }
        let mac = Target::new(target::Arch::Aarch64, Os::MacOS);
        let args = Args::parse_from(run_preprocess(&["foo.c"]));
        assert_eq!(position_independence(&args, &mac), P::Absolute);
    }

    #[test]
    fn test_preprocess_pic_family_keeps_its_spelling() {
        for flag in ["-fpic", "-fPIC", "-fpie", "-fPIE", "-fno-pic", "-fno-PIE"] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(result.contains(&format!("--c17-pic={flag}")), "{result:?}");
            assert!(!result.contains(&flag.to_string()), "{result:?}");
        }
    }

    #[test]
    fn test_preprocess_pie_is_a_linker_flag() {
        for flag in ["-pie", "-no-pie", "-static-pie"] {
            let result = run_preprocess(&[flag, "foo.c"]);
            assert!(result.contains(&format!("--c17-linker-flag={flag}")));
            assert!(!result.iter().any(|a| a.starts_with("--c17-pic")), "{flag}");
        }
    }

    #[test]
    fn test_preprocess_static_is_a_linker_flag() {
        // Not the short cluster `-s -t -a -t -i -c`.
        let result = run_preprocess(&["-static", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-static".to_string()));
        assert!(!result.contains(&"-static".to_string()));
    }

    /// A static link leads with `-no-pie` whatever PIE request came with it;
    /// only `-static-pie` makes a static PIE. Otherwise the last of `-pie`
    /// and `-no-pie` decides, and without either, position-dependent code is
    /// linked into a position-dependent executable.
    #[test]
    fn test_link_mode_flag() {
        let linux = Target::new(target::Arch::X86_64, Os::Linux);
        let mode = |argv: &[&str]| {
            let args = Args::parse_from(run_preprocess(argv));
            link_mode_flag(&args, &linux)
        };
        assert_eq!(mode(&["foo.c"]), Some("-pie"));
        assert_eq!(mode(&["-no-pie", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-no-pie", "-pie", "foo.c"]), Some("-pie"));
        assert_eq!(mode(&["-pie", "-no-pie", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-fno-pic", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-fno-pic", "-pie", "foo.c"]), Some("-pie"));
        assert_eq!(mode(&["-fPIC", "foo.c"]), Some("-pie"));
        assert_eq!(mode(&["-fPIE", "-no-pie", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-static", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-pie", "-static", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-static", "-fPIE", "foo.c"]), Some("-no-pie"));
        assert_eq!(mode(&["-static-pie", "foo.c"]), Some("-static-pie"));
        assert_eq!(mode(&["-shared", "-static", "foo.c"]), Some("-shared"));
    }

    /// A Mach-O executable on arm64 is always position independent, and
    /// Apple's driver ignores `-pie`/`-no-pie` with "argument unused during
    /// compilation": no executable link mode is passed there, whatever was
    /// asked. A shared library still says so.
    #[test]
    fn test_link_mode_flag_macos() {
        let macos = Target::new(target::Arch::Aarch64, Os::MacOS);
        let mode = |argv: &[&str]| {
            let args = Args::parse_from(run_preprocess(argv));
            link_mode_flag(&args, &macos)
        };
        for argv in [
            &["foo.c"][..],
            &["-fno-pic", "foo.c"],
            &["-no-pie", "foo.c"],
            &["-pie", "foo.c"],
            &["-fPIC", "foo.c"],
            &["-static", "foo.c"],
            &["-static-pie", "foo.c"],
        ] {
            assert_eq!(mode(argv), None, "{argv:?}");
        }
        assert_eq!(mode(&["-shared", "foo.c"]), Some("-shared"));
    }

    // Tests for linker passthrough flags

    #[test]
    fn test_preprocess_wl_flags() {
        // Kept whole: the host driver splits the commas, and the pieces of
        // `-z,now` only mean something together.
        let result = run_preprocess(&["-Wl,-z,now", "foo.c"]);
        assert!(result.contains(&"--c17-linker-flag=-Wl,-z,now".to_string()));
        assert!(!result.contains(&"-Wl,-z,now".to_string()));
    }

    #[test]
    fn test_preprocess_xlinker() {
        let result = run_preprocess(&["-Xlinker", "--hash-style=gnu", "foo.c"]);
        let at = result
            .iter()
            .position(|a| a == "--c17-linker-flag=-Xlinker")
            .expect("-Xlinker kept");
        assert_eq!(result[at + 1], "--c17-linker-flag=--hash-style=gnu");
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

    /// `-fsignaling-nans` defines gcc's `__SUPPORT_SNAN__`, the last of it
    /// and `-fno-signaling-nans` wins, and neither is passed on.
    #[test]
    fn test_preprocess_signaling_nans() {
        let defines = |args: &[&str]| {
            let result = run_preprocess(args);
            assert!(!result.iter().any(|a| a.contains("signaling-nans")));
            result.contains(&"__SUPPORT_SNAN__".to_string())
        };
        assert!(defines(&["-fsignaling-nans", "foo.c"]));
        assert!(!defines(&["foo.c"]));
        assert!(!defines(&[
            "-fsignaling-nans",
            "-fno-signaling-nans",
            "foo.c"
        ]));
        assert!(defines(&[
            "-fno-signaling-nans",
            "-fsignaling-nans",
            "foo.c"
        ]));
        // Ahead of `--`, after which everything is an operand.
        let result = run_preprocess(&["-fsignaling-nans", "--", "foo.c"]);
        let snan = result.iter().position(|a| a == "__SUPPORT_SNAN__");
        let dashes = result.iter().position(|a| a == "--");
        assert!(snan < dashes, "{result:?}");
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
        assert!(!result.contains(&"-fno-plt".to_string()));
        // The stack protector c17 does not provide is kept for its warning.
        assert!(result.contains(&"--c17-unsupported=-fstack-protector-strong".to_string()));
        assert!(!result.contains(&"-pipe".to_string()));
        // Linker flags should be passed through
        assert!(result.iter().any(|a| a.starts_with("--c17-linker-flag=")));
    }
}
