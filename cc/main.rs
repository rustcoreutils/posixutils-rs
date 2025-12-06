//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// pcc - A POSIX C99 compiler
//

mod arch;
mod diag;
mod dominate;
mod ir;
mod linearize;
mod lower;
mod os;
mod parse;
mod ssa;
mod symbol;
mod target;
mod token;
mod types;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::fs::File;
use std::io::{self, BufReader, Read, Write};
use std::path::Path;
use std::process::Command;

use parse::Parser as CParser;
use symbol::SymbolTable;
use target::Target;
use token::{preprocess, show_token, token_type_name, StreamTable, Tokenizer};

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("pcc - compile standard C programs"))]
struct Args {
    /// Input files
    #[arg(required = true)]
    files: Vec<String>,

    /// Dump tokens (for debugging tokenizer)
    #[arg(
        short = 'T',
        long = "dump-tokens",
        help = gettext("Dump tokens to stdout")
    )]
    dump_tokens: bool,

    /// Run preprocessor and dump result
    #[arg(short = 'E', help = gettext("Preprocess only, output to stdout"))]
    preprocess_only: bool,

    /// Dump AST (for debugging parser)
    #[arg(
        short = 'A',
        long = "dump-ast",
        help = gettext("Parse and dump AST to stdout")
    )]
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

    /// Define a macro (-D name or -D name=value)
    #[arg(short = 'D', action = clap::ArgAction::Append, value_name = "macro")]
    defines: Vec<String>,

    /// Undefine a macro
    #[arg(short = 'U', action = clap::ArgAction::Append, value_name = "macro")]
    undefines: Vec<String>,

    /// Add include path
    #[arg(short = 'I', action = clap::ArgAction::Append, value_name = "dir")]
    include_paths: Vec<String>,

    /// Compile and assemble, but do not link
    #[arg(short = 'c', help = gettext("Compile and assemble, but do not link"))]
    compile_only: bool,

    /// Compile only; do not assemble or link (output assembly)
    #[arg(short = 'S', help = gettext("Compile only; output assembly"))]
    asm_only: bool,

    /// Place output in file
    #[arg(short = 'o', value_name = "file", help = gettext("Place output in file"))]
    output: Option<String>,

    /// Dump generated assembly (for debugging codegen)
    #[arg(long = "dump-asm", help = gettext("Dump generated assembly to stdout"))]
    dump_asm: bool,

    /// Generate debug information (DWARF)
    #[arg(short = 'g', help = gettext("Generate debug information"))]
    debug: bool,

    /// Disable CFI unwind tables (enabled by default)
    #[arg(long = "fno-unwind-tables", help = gettext("Disable CFI unwind table generation"))]
    no_unwind_tables: bool,
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

    // Tokenize
    let mut tokenizer = Tokenizer::new(&buffer, stream_id);
    let tokens = tokenizer.tokenize();

    // Dump raw tokens if requested
    if args.dump_tokens && !args.preprocess_only {
        let idents = tokenizer.ident_table();
        for token in &tokens {
            if args.verbose {
                println!(
                    "{:>4}:{:<3} {:12} {}",
                    token.pos.line,
                    token.pos.col,
                    token_type_name(token.typ),
                    show_token(token, idents)
                );
            } else {
                let text = show_token(token, idents);
                if !text.starts_with('<') {
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
    let preprocessed = preprocess(tokens, target, tokenizer.ident_table_mut(), path);
    let idents = tokenizer.ident_table();

    if args.preprocess_only {
        // Output preprocessed tokens
        for token in &preprocessed {
            if args.verbose {
                println!(
                    "{:>4}:{:<3} {:12} {}",
                    token.pos.line,
                    token.pos.col,
                    token_type_name(token.typ),
                    show_token(token, idents)
                );
            } else {
                let text = show_token(token, idents);
                if !text.starts_with('<') {
                    print!("{} ", text);
                }
            }
        }
        if !args.verbose {
            println!();
        }
        return Ok(());
    }

    // Create symbol table BEFORE parsing
    // symbols are bound during parsing
    let mut symbols = SymbolTable::new();

    // Parse (this also binds symbols to the symbol table)
    let mut parser = CParser::new(&preprocessed, idents, &mut symbols);
    let ast = parser
        .parse_translation_unit()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {}", e)))?;

    if args.dump_ast {
        println!("{:#?}", ast);
        return Ok(());
    }

    // Linearize to IR
    let mut module =
        linearize::linearize_with_debug(&ast, &symbols, args.debug, Some(display_path));

    if args.dump_ir {
        print!("{}", module);
        return Ok(());
    }

    // Lower IR (phi elimination, etc.)
    lower::lower_module(&mut module);

    // Generate assembly
    let emit_unwind_tables = !args.no_unwind_tables;
    let mut codegen =
        arch::codegen::create_codegen_with_options(target.clone(), emit_unwind_tables);
    let asm = codegen.generate(&module);

    if args.dump_asm {
        print!("{}", asm);
        return Ok(());
    }

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
        let mut file = File::create(&asm_file)?;
        file.write_all(asm.as_bytes())?;
        if args.verbose {
            eprintln!("Wrote assembly to {}", asm_file);
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
            return Err(io::Error::new(io::ErrorKind::Other, "assembler failed"));
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
        return Err(io::Error::new(io::ErrorKind::Other, "assembler failed"));
    }

    // Link
    let status = Command::new("cc")
        .args(["-o", &exe_file, &temp_obj])
        .status()?;

    let _ = std::fs::remove_file(&temp_obj);

    if !status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, "linker failed"));
    }

    if args.verbose {
        eprintln!("Wrote executable to {}", exe_file);
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    // Detect target
    let target = Target::host();

    let mut streams = StreamTable::new();

    for path in &args.files {
        if let Err(e) = process_file(path, &mut streams, &args, &target) {
            eprintln!("pcc: {}: {}", path, e);
            std::process::exit(1);
        }
    }

    Ok(())
}
