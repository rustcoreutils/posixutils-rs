//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// cxref - generate a C-language program cross-reference table
//
// POSIX cxref utility: analyzes C source files and builds a cross-reference
// table of all symbols, showing where they are defined and used.
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_cc::parse::ast::{ExprKind, ExternalDecl, Stmt};
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::strings::StringTable;
use posixutils_cc::symbol::SymbolTable;
use posixutils_cc::target::Target;
use posixutils_cc::token::{preprocess_with_defines, PreprocessConfig, StreamTable, Tokenizer};
use posixutils_cc::types::TypeTable;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{self, BufReader, Read, Write};
use std::path::Path;
use std::process::ExitCode;

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("cxref - generate a C-language program cross-reference table"))]
struct Args {
    /// Write combined cross-reference of all input files
    #[arg(short = 'c', long, help = gettext("Combine all input files"))]
    combined: bool,

    /// Operate silently (do not print input filenames)
    #[arg(short = 's', long, help = gettext("Silent mode, suppress filenames"))]
    silent: bool,

    /// Direct output to named file
    #[arg(short = 'o', long, help = gettext("Write output to file"))]
    output: Option<String>,

    /// Format output no wider than num columns
    #[arg(short = 'w', long, default_value = "80", help = gettext("Maximum output width"))]
    width: usize,

    #[arg(short = 'D', action = clap::ArgAction::Append, help = gettext("Preprocessor defines"))]
    defines: Vec<String>,

    #[arg(short = 'I', action = clap::ArgAction::Append, help = gettext("Include paths"))]
    include_paths: Vec<String>,

    #[arg(short = 'U', action = clap::ArgAction::Append, help = gettext("Undefine macros"))]
    undefines: Vec<String>,

    #[arg(required = true, help = gettext("Input files"))]
    files: Vec<String>,
}

// ============================================================================
// Symbol Reference
// ============================================================================

/// A reference to a symbol
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct SymbolRef {
    /// Line number
    line: u32,
    /// Is this the declaring/defining reference?
    is_definition: bool,
}

/// Information about a symbol across files
#[derive(Debug, Clone)]
struct SymbolInfo {
    /// References organized by file, then by function scope
    /// Map: file -> Map: function (or empty for global) -> refs
    refs: BTreeMap<String, BTreeMap<String, BTreeSet<SymbolRef>>>,
}

impl SymbolInfo {
    fn new() -> Self {
        Self {
            refs: BTreeMap::new(),
        }
    }

    fn add_ref(&mut self, file: &str, function: &str, line: u32, is_definition: bool) {
        self.refs
            .entry(file.to_string())
            .or_default()
            .entry(function.to_string())
            .or_default()
            .insert(SymbolRef {
                line,
                is_definition,
            });
    }
}

/// Cross-reference table
struct CrossRef {
    /// All symbols: name -> info
    symbols: BTreeMap<String, SymbolInfo>,
    /// Current file being processed
    current_file: String,
    /// Current function scope (empty string for global)
    current_function: String,
}

impl CrossRef {
    fn new() -> Self {
        Self {
            symbols: BTreeMap::new(),
            current_file: String::new(),
            current_function: String::new(),
        }
    }

    fn set_file(&mut self, file: &str) {
        self.current_file = file.to_string();
    }

    fn set_function(&mut self, func: &str) {
        self.current_function = func.to_string();
    }

    fn add_definition(&mut self, name: &str, line: u32) {
        self.symbols
            .entry(name.to_string())
            .or_insert_with(SymbolInfo::new)
            .add_ref(&self.current_file, &self.current_function, line, true);
    }

    fn add_reference(&mut self, name: &str, line: u32) {
        self.symbols
            .entry(name.to_string())
            .or_insert_with(SymbolInfo::new)
            .add_ref(&self.current_file, &self.current_function, line, false);
    }
}

// ============================================================================
// AST Walking
// ============================================================================

/// Walk an expression to find symbol references
fn extract_refs_from_expr(
    expr: &posixutils_cc::parse::ast::Expr,
    strings: &StringTable,
    symbols: &SymbolTable,
    xref: &mut CrossRef,
) {
    match &expr.kind {
        ExprKind::Ident(symbol_id) => {
            let name_id = symbols.get(*symbol_id).name;
            let sym_name = strings.get(name_id).to_string();
            xref.add_reference(&sym_name, expr.pos.line);
        }
        ExprKind::FuncName => {
            // __func__ is a special identifier, not a user-defined symbol
        }
        ExprKind::Call { func, args } => {
            extract_refs_from_expr(func, strings, symbols, xref);
            for arg in args {
                extract_refs_from_expr(arg, strings, symbols, xref);
            }
        }
        ExprKind::Unary { operand, .. } => {
            extract_refs_from_expr(operand, strings, symbols, xref);
        }
        ExprKind::Binary { left, right, .. } => {
            extract_refs_from_expr(left, strings, symbols, xref);
            extract_refs_from_expr(right, strings, symbols, xref);
        }
        ExprKind::Assign { target, value, .. } => {
            extract_refs_from_expr(target, strings, symbols, xref);
            extract_refs_from_expr(value, strings, symbols, xref);
        }
        ExprKind::PostInc(e) | ExprKind::PostDec(e) => {
            extract_refs_from_expr(e, strings, symbols, xref);
        }
        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            extract_refs_from_expr(cond, strings, symbols, xref);
            extract_refs_from_expr(then_expr, strings, symbols, xref);
            extract_refs_from_expr(else_expr, strings, symbols, xref);
        }
        ExprKind::Member { expr, .. } | ExprKind::Arrow { expr, .. } => {
            extract_refs_from_expr(expr, strings, symbols, xref);
        }
        ExprKind::Index { array, index } => {
            extract_refs_from_expr(array, strings, symbols, xref);
            extract_refs_from_expr(index, strings, symbols, xref);
        }
        ExprKind::Cast { expr, .. } => {
            extract_refs_from_expr(expr, strings, symbols, xref);
        }
        ExprKind::SizeofExpr(e) | ExprKind::AlignofExpr(e) => {
            extract_refs_from_expr(e, strings, symbols, xref);
        }
        ExprKind::Comma(exprs) => {
            for e in exprs {
                extract_refs_from_expr(e, strings, symbols, xref);
            }
        }
        ExprKind::InitList { elements } | ExprKind::CompoundLiteral { elements, .. } => {
            for elem in elements {
                extract_refs_from_expr(&elem.value, strings, symbols, xref);
            }
        }
        ExprKind::VaStart { ap, .. } => {
            extract_refs_from_expr(ap, strings, symbols, xref);
        }
        ExprKind::VaArg { ap, .. } => {
            extract_refs_from_expr(ap, strings, symbols, xref);
        }
        ExprKind::VaEnd { ap } => {
            extract_refs_from_expr(ap, strings, symbols, xref);
        }
        ExprKind::VaCopy { dest, src } => {
            extract_refs_from_expr(dest, strings, symbols, xref);
            extract_refs_from_expr(src, strings, symbols, xref);
        }
        ExprKind::Bswap16 { arg }
        | ExprKind::Bswap32 { arg }
        | ExprKind::Bswap64 { arg }
        | ExprKind::Ctz { arg }
        | ExprKind::Ctzl { arg }
        | ExprKind::Ctzll { arg }
        | ExprKind::Clz { arg }
        | ExprKind::Clzl { arg }
        | ExprKind::Clzll { arg }
        | ExprKind::Popcount { arg }
        | ExprKind::Popcountl { arg }
        | ExprKind::Popcountll { arg }
        | ExprKind::Alloca { size: arg }
        | ExprKind::FrameAddress { level: arg }
        | ExprKind::ReturnAddress { level: arg } => {
            extract_refs_from_expr(arg, strings, symbols, xref);
        }
        _ => {}
    }
}

/// Walk a statement to find symbol references
fn extract_refs_from_stmt(
    stmt: &Stmt,
    strings: &StringTable,
    symbols: &SymbolTable,
    xref: &mut CrossRef,
) {
    match stmt {
        Stmt::Empty => {}
        Stmt::Expr(expr) => {
            extract_refs_from_expr(expr, strings, symbols, xref);
        }
        Stmt::Block(items) => {
            for item in items {
                match item {
                    posixutils_cc::parse::ast::BlockItem::Statement(s) => {
                        extract_refs_from_stmt(s, strings, symbols, xref);
                    }
                    posixutils_cc::parse::ast::BlockItem::Declaration(decl) => {
                        for d in &decl.declarators {
                            let name = strings.get(symbols.get(d.symbol).name).to_string();
                            // Local variable definition
                            // Note: we don't have position for declarations, use 0
                            if let Some(init) = &d.init {
                                xref.add_definition(&name, init.pos.line);
                                extract_refs_from_expr(init, strings, symbols, xref);
                            }
                        }
                    }
                }
            }
        }
        Stmt::If {
            cond,
            then_stmt,
            else_stmt,
        } => {
            extract_refs_from_expr(cond, strings, symbols, xref);
            extract_refs_from_stmt(then_stmt, strings, symbols, xref);
            if let Some(else_s) = else_stmt {
                extract_refs_from_stmt(else_s, strings, symbols, xref);
            }
        }
        Stmt::While { cond, body } => {
            extract_refs_from_expr(cond, strings, symbols, xref);
            extract_refs_from_stmt(body, strings, symbols, xref);
        }
        Stmt::DoWhile { body, cond } => {
            extract_refs_from_stmt(body, strings, symbols, xref);
            extract_refs_from_expr(cond, strings, symbols, xref);
        }
        Stmt::For {
            init,
            cond,
            post,
            body,
        } => {
            if let Some(i) = init {
                match i {
                    posixutils_cc::parse::ast::ForInit::Expression(e) => {
                        extract_refs_from_expr(e, strings, symbols, xref);
                    }
                    posixutils_cc::parse::ast::ForInit::Declaration(d) => {
                        for decl in &d.declarators {
                            let name = strings.get(symbols.get(decl.symbol).name).to_string();
                            if let Some(init_expr) = &decl.init {
                                xref.add_definition(&name, init_expr.pos.line);
                                extract_refs_from_expr(init_expr, strings, symbols, xref);
                            }
                        }
                    }
                }
            }
            if let Some(c) = cond {
                extract_refs_from_expr(c, strings, symbols, xref);
            }
            if let Some(p) = post {
                extract_refs_from_expr(p, strings, symbols, xref);
            }
            extract_refs_from_stmt(body, strings, symbols, xref);
        }
        Stmt::Return(Some(expr)) => {
            extract_refs_from_expr(expr, strings, symbols, xref);
        }
        Stmt::Switch { expr, body } => {
            extract_refs_from_expr(expr, strings, symbols, xref);
            extract_refs_from_stmt(body, strings, symbols, xref);
        }
        Stmt::Case(expr) => {
            extract_refs_from_expr(expr, strings, symbols, xref);
        }
        Stmt::Label { stmt, .. } => {
            extract_refs_from_stmt(stmt, strings, symbols, xref);
        }
        _ => {}
    }
}

// ============================================================================
// File Processing
// ============================================================================

fn process_file(
    path: &str,
    streams: &mut StreamTable,
    xref: &mut CrossRef,
    defines: &[String],
    undefines: &[String],
    include_paths: &[String],
) -> io::Result<()> {
    // Read file content
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;

    xref.set_file(path);

    // Create stream
    let stream_id = streams.add(path.to_string());

    // Create string table for identifier interning
    let mut strings = StringTable::new();

    // Tokenize
    let tokens = {
        let mut tokenizer = Tokenizer::new(&buffer, stream_id, &mut strings);
        tokenizer.tokenize()
    };

    // Preprocess
    let target = Target::host();
    let preprocessed = preprocess_with_defines(
        tokens,
        &target,
        &mut strings,
        path,
        &PreprocessConfig {
            defines,
            undefines,
            include_paths,
            no_std_inc: false,
            no_builtin_inc: false,
        },
    );

    // Create symbol table and type table
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&target);

    // Parse
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = match parser.parse_translation_unit() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("cxref: {}: parse error: {}", path, e);
            return Ok(());
        }
    };

    // Extract references from AST
    for item in &ast.items {
        match item {
            ExternalDecl::FunctionDef(func) => {
                let name = strings.get(func.name).to_string();
                xref.set_function(&name);
                xref.add_definition(&name, func.pos.line);

                // Add parameter references
                for param in &func.params {
                    if let Some(symbol_id) = param.symbol {
                        let pname = strings.get(symbols.get(symbol_id).name).to_string();
                        xref.add_definition(&pname, func.pos.line);
                    }
                }

                // Process function body
                extract_refs_from_stmt(&func.body, &strings, &symbols, xref);
                xref.set_function("");
            }
            ExternalDecl::Declaration(decl) => {
                xref.set_function("");
                for d in &decl.declarators {
                    let name = strings.get(symbols.get(d.symbol).name).to_string();
                    // Use initializer position if available, otherwise 0
                    // (Declaration/InitDeclarator don't carry position info)
                    let line = d.init.as_ref().map(|e| e.pos.line).unwrap_or(0);
                    xref.add_definition(&name, line);
                    if let Some(init) = &d.init {
                        extract_refs_from_expr(init, &strings, &symbols, xref);
                    }
                }
            }
        }
    }

    Ok(())
}

// ============================================================================
// Output
// ============================================================================

/// Format and print the cross-reference
fn print_xref(xref: &CrossRef, width: usize, silent: bool, output: &mut dyn Write) {
    for (name, info) in &xref.symbols {
        let mut first_file = true;

        for (file, funcs) in &info.refs {
            let file_display = if silent {
                String::new()
            } else {
                format!("{:<20}", file)
            };

            for (func, refs) in funcs {
                let func_display = if func.is_empty() {
                    "".to_string()
                } else {
                    format!("{:<15}", func)
                };

                // Format line numbers, respecting width limit
                let mut line_parts = Vec::new();
                for r in refs {
                    let line_str = if r.is_definition {
                        format!("*{}", r.line)
                    } else {
                        format!("{}", r.line)
                    };
                    line_parts.push(line_str);
                }

                // Calculate prefix length for continuation lines
                let prefix = format!(
                    "{:<16} {} {} ",
                    if first_file { name.as_str() } else { "" },
                    file_display,
                    func_display
                );
                let prefix_len = prefix.len();

                // Build output lines respecting width
                let mut current_line = prefix.clone();
                let mut first_num = true;

                for part in &line_parts {
                    let needed = if first_num {
                        part.len()
                    } else {
                        part.len() + 1 // space separator
                    };

                    if !first_num && current_line.len() + needed > width {
                        // Flush current line and start continuation
                        let _ = writeln!(output, "{}", current_line);
                        current_line = format!("{:prefix_len$}", "");
                        first_num = true;
                    }

                    if first_num {
                        current_line.push_str(part);
                        first_num = false;
                    } else {
                        current_line.push(' ');
                        current_line.push_str(part);
                    }
                }

                if !first_num {
                    let _ = writeln!(output, "{}", current_line);
                }

                first_file = false;
            }
        }
    }
}

// ============================================================================
// Main
// ============================================================================

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").unwrap();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").unwrap();

    let args = Args::parse();

    // Determine output
    let stdout = io::stdout();
    let mut output_file: Box<dyn Write>;

    if let Some(ref path) = args.output {
        match File::create(path) {
            Ok(f) => {
                output_file = Box::new(f);
            }
            Err(e) => {
                eprintln!("cxref: cannot open {}: {}", path, e);
                return ExitCode::from(1);
            }
        }
    } else {
        output_file = Box::new(stdout.lock());
    }

    // Build cross-reference
    let mut xref = CrossRef::new();
    let mut streams = StreamTable::new();

    for file in &args.files {
        let ext = Path::new(file)
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        match ext {
            "c" | "h" => {
                if let Err(e) = process_file(
                    file,
                    &mut streams,
                    &mut xref,
                    &args.defines,
                    &args.undefines,
                    &args.include_paths,
                ) {
                    eprintln!("cxref: {}: {}", file, e);
                }

                // In non-combined mode, print and reset after each file
                if !args.combined {
                    print_xref(&xref, args.width, args.silent, &mut *output_file);
                    xref = CrossRef::new();
                }
            }
            _ => {
                eprintln!("cxref: {}: not a C source file", file);
            }
        }
    }

    // In combined mode, print all at end
    if args.combined {
        print_xref(&xref, args.width, args.silent, &mut *output_file);
    }

    ExitCode::SUCCESS
}
