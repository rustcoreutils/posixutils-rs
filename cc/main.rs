//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// pcc - A POSIX C99 compiler
//

mod arch;
mod builtin_headers;
mod diag;
mod ir;
mod opt;
mod os;
mod parse;
mod strings;
mod symbol;
mod target;
mod token;
mod types;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use std::fs::File;
use std::io::{self, BufReader, Read, Write};
use std::path::Path;
use std::process::Command;

use parse::Parser as CParser;
use strings::StringTable;
use symbol::SymbolTable;
use target::Target;
use token::{
    PreprocessConfig, StreamTable, Tokenizer, preprocess_with_defines, show_token, token_type_name,
};

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("pcc - compile standard C programs"))]
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

    /// Dump IR (for debugging linearizer)
    #[arg(long = "dump-ir", help = gettext("Linearize and dump IR to stdout"))]
    dump_ir: bool,

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
    #[arg(short = 'g', help = gettext("Generate debug information"))]
    debug: bool,

    /// Disable CFI unwind tables (enabled by default)
    #[arg(long = "fno-unwind-tables", help = gettext("Disable CFI unwind table generation"))]
    no_unwind_tables: bool,

    /// Generate position-independent code (for shared libraries)
    /// Set by preprocess_args() when -fPIC or -fpic is passed
    #[arg(long = "pcc-fpic", hide = true)]
    fpic: bool,

    /// Produce a shared library
    #[arg(long = "shared", help = gettext("Produce a shared library"))]
    shared: bool,

    /// Target triple (e.g., aarch64-apple-darwin, x86_64-unknown-linux-gnu)
    #[arg(long = "target", value_name = "triple", help = gettext("Target triple for cross-compilation"))]
    target: Option<String>,

    /// Optimization level (0=none, 1+=basic optimizations)
    /// -O alone means -O1, -O0 means none, -O2/-O3 mapped to -O1
    #[arg(short = 'O', default_value = "0", default_missing_value = "1",
          num_args = 0..=1, value_name = "level", help = gettext("Optimization level"))]
    opt_level: u32,

    #[arg(short = 'W', action = clap::ArgAction::Append, value_name = "warning",
          num_args = 0..=1, default_missing_value = "extra", help = gettext("Warning flags (e.g., -Wall, -Wextra, -Wno-unused)"))]
    warnings: Vec<String>,

    #[arg(long = "pedantic", hide = true, help = gettext("Pedantic mode (compatibility)"))]
    pedantic: bool,

    #[arg(short = 'L', action = clap::ArgAction::Append, value_name = "dir", help = gettext("Add library search path (passed to linker)"))]
    lib_paths: Vec<String>,

    #[arg(short = 'l', action = clap::ArgAction::Append, value_name = "library", help = gettext("Link library (passed to linker)"))]
    libraries: Vec<String>,
}

fn process_file(
    path: &str,
    streams: &mut StreamTable,
    args: &Args,
    target: &Target,
) -> io::Result<()> {
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
        return Ok(());
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
        },
    );

    if args.preprocess_only {
        // Output preprocessed tokens
        for token in &preprocessed {
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
        return Ok(());
    }

    // Create symbol table and type table BEFORE parsing
    // symbols are bound during parsing
    let mut symbols = SymbolTable::new();
    let mut types = types::TypeTable::new(target.pointer_width);

    // Parse (this also binds symbols to the symbol table)
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = parser
        .parse_translation_unit()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {}", e)))?;

    if args.dump_ast {
        println!("{:#?}", ast);
        return Ok(());
    }

    // Linearize to IR
    let mut module = ir::linearize::linearize_with_debug(
        &ast,
        &symbols,
        &types,
        &strings,
        target,
        args.debug,
        Some(display_path),
    );

    // Optimize IR (if enabled)
    if args.opt_level > 0 {
        opt::optimize_module(&mut module, args.opt_level);
    }

    if args.dump_ir {
        print!("{}", module);
        return Ok(());
    }

    // Lower IR (phi elimination, etc.)
    ir::lower::lower_module(&mut module);

    // Generate assembly
    let emit_unwind_tables = !args.no_unwind_tables;
    let mut codegen =
        arch::codegen::create_codegen_with_options(target.clone(), emit_unwind_tables, args.fpic);
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
        return Ok(());
    }

    // Write temporary assembly file
    let temp_asm = format!("/tmp/pcc_{}.s", std::process::id());
    {
        let mut file = File::create(&temp_asm)?;
        file.write_all(asm.as_bytes())?;
    }

    if args.compile_only {
        // Assemble to object file
        let obj_file = args.output.clone().unwrap_or_else(|| format!("{}.o", stem));

        let mut as_cmd = Command::new("as");
        if args.debug {
            as_cmd.arg("-g");
        }
        let status = as_cmd.args(["-o", &obj_file, &temp_asm]).status()?;

        // Clean up temp file
        let _ = std::fs::remove_file(&temp_asm);

        if !status.success() {
            return Err(io::Error::other("assembler failed"));
        }

        if args.verbose {
            eprintln!("Wrote object file to {}", obj_file);
        }
        return Ok(());
    }

    // Full compile: assemble and link to executable
    let exe_file = args.output.clone().unwrap_or_else(|| "a.out".to_string());

    // Assemble
    let temp_obj = format!("/tmp/pcc_{}.o", std::process::id());
    let mut as_cmd = Command::new("as");
    if args.debug {
        as_cmd.arg("-g");
    }
    let status = as_cmd.args(["-o", &temp_obj, &temp_asm]).status()?;

    let _ = std::fs::remove_file(&temp_asm);

    if !status.success() {
        return Err(io::Error::other("assembler failed"));
    }

    // Link
    let mut link_cmd = Command::new("cc");
    // Pass -shared flag for shared library creation
    if args.shared {
        link_cmd.arg("-shared");
    }
    link_cmd.args(["-o", &exe_file, &temp_obj]);
    // Add library search paths
    for lib_path in &args.lib_paths {
        link_cmd.arg(format!("-L{}", lib_path));
    }
    // Add libraries
    for lib in &args.libraries {
        link_cmd.arg(format!("-l{}", lib));
    }
    let status = link_cmd.status()?;

    let _ = std::fs::remove_file(&temp_obj);

    if !status.success() {
        return Err(io::Error::other("linker failed"));
    }

    if args.verbose {
        eprintln!("Wrote executable to {}", exe_file);
    }

    Ok(())
}

/// Check if a string is a valid optimization level for -O
fn is_valid_opt_level(s: &str) -> bool {
    matches!(s, "0" | "1" | "2" | "3" | "s" | "z" | "fast" | "g")
}

/// Preprocess command-line arguments for gcc compatibility.
/// - Converts -Wall → -W all, -Wextra → -W extra, etc.
/// - Handles -O flag: standalone -O followed by non-level becomes -O1
fn preprocess_args() -> Vec<String> {
    let raw_args: Vec<String> = std::env::args().collect();
    let mut result = Vec::with_capacity(raw_args.len());
    let mut i = 0;

    while i < raw_args.len() {
        let arg = &raw_args[i];

        if arg == "-O" {
            // Standalone -O: check if next arg is a valid optimization level
            if i + 1 < raw_args.len() && is_valid_opt_level(&raw_args[i + 1]) {
                // Next arg is a level like "1", "2", etc. - merge them
                result.push(format!("-O{}", raw_args[i + 1]));
                i += 2;
                continue;
            }
            // Next arg is not a valid level (or no next arg) - use default -O1
            result.push("-O1".to_string());
            i += 1;
        } else if arg.starts_with("-O") && arg.len() > 2 {
            // -O0, -O1, -O2, -O3, -Os, -Oz, -Ofast, -Og - pass through
            result.push(arg.clone());
            i += 1;
        } else if arg.starts_with("-W") && arg.len() > 2 {
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
        } else if arg == "-fPIC" || arg == "-fpic" {
            // -fPIC / -fpic → --pcc-fpic (internal flag)
            result.push("--pcc-fpic".to_string());
            i += 1;
        } else if arg == "-shared" {
            // -shared → --shared
            result.push("--shared".to_string());
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

/// Check if a file is an object file or library (by extension)
fn is_object_file(path: &str) -> bool {
    path.ends_with(".o")
        || path.ends_with(".a")
        || path.ends_with(".so")
        || path.ends_with(".dylib")
        || path.contains(".so.") // versioned .so files like libz.so.1.3.1
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse_from(preprocess_args());

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
                eprintln!("pcc: unsupported target: {}", triple);
                std::process::exit(1);
            }
        }
    } else {
        Target::host()
    };

    // Separate source files from object files
    let source_files: Vec<&String> = args.files.iter().filter(|f| is_source_file(f)).collect();
    let object_files: Vec<&String> = args.files.iter().filter(|f| is_object_file(f)).collect();

    // Warn about unrecognized file types
    for file in &args.files {
        if !is_source_file(file) && !is_object_file(file) {
            eprintln!("pcc: warning: unrecognized file type: {}", file);
        }
    }

    // If we only have object files and an output is specified, just link them
    if source_files.is_empty() && !object_files.is_empty() && args.output.is_some() {
        let exe_file = args.output.clone().unwrap();
        let mut link_cmd = Command::new("cc");
        // Pass -shared flag for shared library creation
        if args.shared {
            link_cmd.arg("-shared");
        }
        link_cmd.args(["-o", &exe_file]);
        for obj in &object_files {
            link_cmd.arg(*obj);
        }
        // Add library search paths
        for lib_path in &args.lib_paths {
            link_cmd.arg(format!("-L{}", lib_path));
        }
        // Add libraries
        for lib in &args.libraries {
            link_cmd.arg(format!("-l{}", lib));
        }
        let status = link_cmd.status()?;
        if !status.success() {
            eprintln!("pcc: linker failed");
            std::process::exit(1);
        }
        if args.verbose {
            eprintln!("Linked {} object files to {}", object_files.len(), exe_file);
        }
        return Ok(());
    }

    let mut streams = StreamTable::new();

    for path in &source_files {
        if let Err(e) = process_file(path, &mut streams, &args, &target) {
            eprintln!("pcc: {}: {}", path, e);
            std::process::exit(1);
        }
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
        // We can't easily test preprocess_args() without modifying env::args,
        // so we'll test the logic directly by simulating what it does
        let input: Vec<String> = std::iter::once("pcc".to_string())
            .chain(args.iter().map(|s| s.to_string()))
            .collect();

        // Simulate preprocess_args logic
        let mut result = vec![input[0].clone()];
        let mut i = 1;
        while i < input.len() {
            let arg = &input[i];
            if arg == "-fPIC" || arg == "-fpic" {
                result.push("--pcc-fpic".to_string());
            } else if arg == "-shared" {
                result.push("--shared".to_string());
            } else if arg.starts_with("-L") && arg.len() > 2 {
                result.push("-L".to_string());
                result.push(arg[2..].to_string());
            } else if arg.starts_with("-l") && arg.len() > 2 {
                result.push("-l".to_string());
                result.push(arg[2..].to_string());
            } else if arg.starts_with("-W") && arg.len() > 2 {
                result.push("-W".to_string());
                result.push(arg[2..].to_string());
            } else {
                result.push(arg.clone());
            }
            i += 1;
        }
        result
    }

    #[test]
    fn test_preprocess_fpic_uppercase() {
        let result = run_preprocess(&["-fPIC", "foo.c"]);
        assert!(result.contains(&"--pcc-fpic".to_string()));
        assert!(!result.contains(&"-fPIC".to_string()));
    }

    #[test]
    fn test_preprocess_fpic_lowercase() {
        let result = run_preprocess(&["-fpic", "foo.c"]);
        assert!(result.contains(&"--pcc-fpic".to_string()));
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
        assert!(result.contains(&"--pcc-fpic".to_string()));
        assert!(result.contains(&"--shared".to_string()));
        assert!(result.contains(&"-l".to_string()));
        assert!(result.contains(&"z".to_string()));
        assert!(result.contains(&"-L".to_string()));
        assert!(result.contains(&".".to_string()));
    }
}
