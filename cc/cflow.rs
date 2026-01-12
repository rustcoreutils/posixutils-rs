//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// cflow - generate a C-language flowgraph
//
// POSIX cflow utility: analyzes C source files and builds a graph
// charting the external references (function call relationships).
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_cc::parse::ast::{ExprKind, ExternalDecl, Stmt};
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::strings::StringTable;
use posixutils_cc::symbol::SymbolTable;
use posixutils_cc::target::Target;
use posixutils_cc::token::{preprocess_with_defines, PreprocessConfig, StreamTable, Tokenizer};
use posixutils_cc::types::{TypeKind, TypeTable};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufReader, Read};
use std::path::Path;
use std::process::ExitCode;

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(version, about = gettext("cflow - generate a C-language flowgraph"))]
struct Args {
    /// Reverse the caller:callee relationship
    #[arg(short = 'r', long, help = gettext("Print inverted listing showing callers of each function"))]
    reverse: bool,

    /// Depth cutoff for the flowgraph
    #[arg(short = 'd', long, help = gettext("Cut off flowgraph at specified depth"))]
    depth: Option<usize>,

    /// Include additional symbols (x=data symbols, _=underscore names)
    #[arg(short = 'i', action = clap::ArgAction::Append, help = gettext("Include additional symbols: x=data, _=underscore names"))]
    include: Vec<String>,

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
// Function Definition Info
// ============================================================================

/// Information about a function definition
#[derive(Debug, Clone)]
struct FunctionInfo {
    /// Function name
    name: String,
    /// Return type as string
    return_type: String,
    /// Source file
    file: String,
    /// Line number
    line: u32,
    /// Functions called by this function
    calls: Vec<String>,
}

/// Global call graph
struct CallGraph {
    /// Function definitions: name -> info
    functions: HashMap<String, FunctionInfo>,
    /// All known function names (defined or called)
    all_names: HashSet<String>,
    /// Include data symbols (for future -i x support)
    _include_data: bool,
    /// Include underscore names
    include_underscore: bool,
}

impl CallGraph {
    fn new(include_data: bool, include_underscore: bool) -> Self {
        Self {
            functions: HashMap::new(),
            all_names: HashSet::new(),
            _include_data: include_data,
            include_underscore,
        }
    }

    fn should_include(&self, name: &str) -> bool {
        if name.starts_with('_') && !self.include_underscore {
            return false;
        }
        true
    }

    fn add_function(&mut self, info: FunctionInfo) {
        if self.should_include(&info.name) {
            self.all_names.insert(info.name.clone());
            for call in &info.calls {
                self.all_names.insert(call.clone());
            }
            self.functions.insert(info.name.clone(), info);
        }
    }

    /// Get root functions (not called by any other function)
    fn get_roots(&self) -> Vec<String> {
        let called: HashSet<_> = self
            .functions
            .values()
            .flat_map(|f| f.calls.iter())
            .cloned()
            .collect();

        let mut roots: Vec<_> = self
            .functions
            .keys()
            .filter(|name| !called.contains(*name))
            .cloned()
            .collect();
        roots.sort();
        roots
    }

    /// Get callers of a function (for reverse mode)
    fn get_callers(&self, name: &str) -> Vec<String> {
        let mut callers: Vec<_> = self
            .functions
            .iter()
            .filter(|(_, info)| info.calls.contains(&name.to_string()))
            .map(|(caller, _)| caller.clone())
            .collect();
        callers.sort();
        callers
    }
}

// ============================================================================
// AST Walking
// ============================================================================

/// Walk an expression to find function calls
fn extract_calls_from_expr(
    expr: &posixutils_cc::parse::ast::Expr,
    strings: &StringTable,
    symbols: &SymbolTable,
    calls: &mut Vec<String>,
) {
    match &expr.kind {
        ExprKind::Call { func, args } => {
            // Extract callee name
            if let ExprKind::Ident(symbol_id) = &func.kind {
                let callee = strings.get(symbols.get(*symbol_id).name).to_string();
                if !calls.contains(&callee) {
                    calls.push(callee);
                }
            }
            // Recurse into function expression and arguments
            extract_calls_from_expr(func, strings, symbols, calls);
            for arg in args {
                extract_calls_from_expr(arg, strings, symbols, calls);
            }
        }
        ExprKind::Unary { operand, .. } => {
            extract_calls_from_expr(operand, strings, symbols, calls);
        }
        ExprKind::Binary { left, right, .. } => {
            extract_calls_from_expr(left, strings, symbols, calls);
            extract_calls_from_expr(right, strings, symbols, calls);
        }
        ExprKind::Assign { target, value, .. } => {
            extract_calls_from_expr(target, strings, symbols, calls);
            extract_calls_from_expr(value, strings, symbols, calls);
        }
        ExprKind::PostInc(e) | ExprKind::PostDec(e) => {
            extract_calls_from_expr(e, strings, symbols, calls);
        }
        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            extract_calls_from_expr(cond, strings, symbols, calls);
            extract_calls_from_expr(then_expr, strings, symbols, calls);
            extract_calls_from_expr(else_expr, strings, symbols, calls);
        }
        ExprKind::Member { expr, .. } | ExprKind::Arrow { expr, .. } => {
            extract_calls_from_expr(expr, strings, symbols, calls);
        }
        ExprKind::Index { array, index } => {
            extract_calls_from_expr(array, strings, symbols, calls);
            extract_calls_from_expr(index, strings, symbols, calls);
        }
        ExprKind::Cast { expr, .. } => {
            extract_calls_from_expr(expr, strings, symbols, calls);
        }
        ExprKind::SizeofExpr(e) => {
            extract_calls_from_expr(e, strings, symbols, calls);
        }
        ExprKind::Comma(exprs) => {
            for e in exprs {
                extract_calls_from_expr(e, strings, symbols, calls);
            }
        }
        ExprKind::InitList { elements } | ExprKind::CompoundLiteral { elements, .. } => {
            for elem in elements {
                extract_calls_from_expr(&elem.value, strings, symbols, calls);
            }
        }
        // Variadic builtins
        ExprKind::VaStart { ap, .. } => {
            extract_calls_from_expr(ap, strings, symbols, calls);
        }
        ExprKind::VaArg { ap, .. } => {
            extract_calls_from_expr(ap, strings, symbols, calls);
        }
        ExprKind::VaEnd { ap } => {
            extract_calls_from_expr(ap, strings, symbols, calls);
        }
        ExprKind::VaCopy { dest, src } => {
            extract_calls_from_expr(dest, strings, symbols, calls);
            extract_calls_from_expr(src, strings, symbols, calls);
        }
        // Other builtins with arguments
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
        | ExprKind::Alloca { size: arg } => {
            extract_calls_from_expr(arg, strings, symbols, calls);
        }
        // Literals, identifiers, and other terminals - no recursion needed
        _ => {}
    }
}

/// Walk a statement to find function calls
fn extract_calls_from_stmt(
    stmt: &Stmt,
    strings: &StringTable,
    symbols: &SymbolTable,
    calls: &mut Vec<String>,
) {
    match stmt {
        Stmt::Empty => {}
        Stmt::Expr(expr) => {
            extract_calls_from_expr(expr, strings, symbols, calls);
        }
        Stmt::Block(items) => {
            for item in items {
                match item {
                    posixutils_cc::parse::ast::BlockItem::Statement(s) => {
                        extract_calls_from_stmt(s, strings, symbols, calls);
                    }
                    posixutils_cc::parse::ast::BlockItem::Declaration(decl) => {
                        for d in &decl.declarators {
                            if let Some(init) = &d.init {
                                extract_calls_from_expr(init, strings, symbols, calls);
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
            extract_calls_from_expr(cond, strings, symbols, calls);
            extract_calls_from_stmt(then_stmt, strings, symbols, calls);
            if let Some(else_s) = else_stmt {
                extract_calls_from_stmt(else_s, strings, symbols, calls);
            }
        }
        Stmt::While { cond, body } => {
            extract_calls_from_expr(cond, strings, symbols, calls);
            extract_calls_from_stmt(body, strings, symbols, calls);
        }
        Stmt::DoWhile { body, cond } => {
            extract_calls_from_stmt(body, strings, symbols, calls);
            extract_calls_from_expr(cond, strings, symbols, calls);
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
                        extract_calls_from_expr(e, strings, symbols, calls);
                    }
                    posixutils_cc::parse::ast::ForInit::Declaration(d) => {
                        for decl in &d.declarators {
                            if let Some(init_expr) = &decl.init {
                                extract_calls_from_expr(init_expr, strings, symbols, calls);
                            }
                        }
                    }
                }
            }
            if let Some(c) = cond {
                extract_calls_from_expr(c, strings, symbols, calls);
            }
            if let Some(p) = post {
                extract_calls_from_expr(p, strings, symbols, calls);
            }
            extract_calls_from_stmt(body, strings, symbols, calls);
        }
        Stmt::Return(Some(expr)) => {
            extract_calls_from_expr(expr, strings, symbols, calls);
        }
        Stmt::Switch { expr, body } => {
            extract_calls_from_expr(expr, strings, symbols, calls);
            extract_calls_from_stmt(body, strings, symbols, calls);
        }
        Stmt::Case(expr) => {
            extract_calls_from_expr(expr, strings, symbols, calls);
        }
        Stmt::Label { stmt, .. } => {
            extract_calls_from_stmt(stmt, strings, symbols, calls);
        }
        _ => {}
    }
}

/// Format a type as a string
fn format_type(typ: posixutils_cc::types::TypeId, types: &TypeTable) -> String {
    let t = types.get(typ);
    let is_unsigned = types.is_unsigned(typ);
    match t.kind {
        TypeKind::Void => "void".to_string(),
        TypeKind::Bool => "_Bool".to_string(),
        TypeKind::Char => {
            if is_unsigned {
                "unsigned char".to_string()
            } else {
                "char".to_string()
            }
        }
        TypeKind::Short => {
            if is_unsigned {
                "unsigned short".to_string()
            } else {
                "short".to_string()
            }
        }
        TypeKind::Int => {
            if is_unsigned {
                "unsigned int".to_string()
            } else {
                "int".to_string()
            }
        }
        TypeKind::Long => {
            if is_unsigned {
                "unsigned long".to_string()
            } else {
                "long".to_string()
            }
        }
        TypeKind::LongLong => {
            if is_unsigned {
                "unsigned long long".to_string()
            } else {
                "long long".to_string()
            }
        }
        TypeKind::Float => "float".to_string(),
        TypeKind::Double => "double".to_string(),
        TypeKind::LongDouble => "long double".to_string(),
        TypeKind::Pointer => {
            if let Some(base) = t.base {
                format!("{} *", format_type(base, types))
            } else {
                "void *".to_string()
            }
        }
        TypeKind::Array => {
            if let Some(base) = t.base {
                format!("{}[]", format_type(base, types))
            } else {
                "[]".to_string()
            }
        }
        TypeKind::Function => {
            if let Some(ret) = t.base {
                format!("{}()", format_type(ret, types))
            } else {
                "()".to_string()
            }
        }
        TypeKind::Struct => "struct".to_string(),
        TypeKind::Union => "union".to_string(),
        TypeKind::Enum => "enum".to_string(),
        TypeKind::VaList => "__builtin_va_list".to_string(),
    }
}

// ============================================================================
// File Processing
// ============================================================================

fn process_file(
    path: &str,
    streams: &mut StreamTable,
    graph: &mut CallGraph,
    defines: &[String],
    undefines: &[String],
    include_paths: &[String],
) -> io::Result<()> {
    // Read file content
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;

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
    let mut types = TypeTable::new(target.pointer_width);

    // Parse
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = match parser.parse_translation_unit() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("cflow: {}: parse error: {}", path, e);
            return Ok(());
        }
    };

    // Extract function definitions and calls
    for item in &ast.items {
        if let ExternalDecl::FunctionDef(func) = item {
            let name = strings.get(func.name).to_string();
            let return_type = format_type(func.return_type, &types);
            let line = func.pos.line;

            let mut calls = Vec::new();
            extract_calls_from_stmt(&func.body, &strings, &symbols, &mut calls);

            // Filter calls based on include options (e.g., underscore names)
            calls.retain(|c| graph.should_include(c));

            let info = FunctionInfo {
                name,
                return_type,
                file: path.to_string(),
                line,
                calls,
            };

            graph.add_function(info);
        }
    }

    Ok(())
}

// ============================================================================
// Output
// ============================================================================

/// Print flowgraph starting from a root function
fn print_flowgraph(
    graph: &CallGraph,
    name: &str,
    depth: usize,
    max_depth: Option<usize>,
    visited: &mut HashSet<String>,
    ref_num: &mut u32,
    printed_refs: &mut HashMap<String, u32>,
) {
    // Check depth limit
    if let Some(max) = max_depth {
        if depth > max {
            return;
        }
    }

    let indent = "    ".repeat(depth);
    *ref_num += 1;
    let my_ref = *ref_num;

    if let Some(info) = graph.functions.get(name) {
        // Check if already printed (show reference instead)
        if let Some(prev_ref) = printed_refs.get(name) {
            println!("{}{} {}  {{{}}}", my_ref, indent, name, prev_ref);
            return;
        }

        printed_refs.insert(name.to_string(), my_ref);

        // Print function definition
        println!(
            "{}{} {}: {}(), <{} {}>",
            my_ref, indent, name, info.return_type, info.file, info.line
        );

        // Avoid infinite recursion
        if visited.contains(name) {
            return;
        }
        visited.insert(name.to_string());

        // Print callees
        for callee in &info.calls {
            print_flowgraph(
                graph,
                callee,
                depth + 1,
                max_depth,
                visited,
                ref_num,
                printed_refs,
            );
        }

        visited.remove(name);
    } else {
        // Undefined function
        println!("{}{} {}: <>", my_ref, indent, name);
    }
}

/// Print reverse flowgraph (callers of each function)
fn print_reverse_flowgraph(graph: &CallGraph) {
    // Get all function names sorted
    let mut all_names: Vec<_> = graph.all_names.iter().cloned().collect();
    all_names.sort();

    let mut ref_num: u32 = 0;

    for name in all_names {
        ref_num += 1;

        if let Some(info) = graph.functions.get(&name) {
            println!(
                "{} {}: {}(), <{} {}>",
                ref_num, name, info.return_type, info.file, info.line
            );
        } else {
            println!("{} {}: <>", ref_num, name);
        }

        // Print callers
        let callers = graph.get_callers(&name);
        for caller in callers {
            ref_num += 1;
            if let Some(info) = graph.functions.get(&caller) {
                println!(
                    "{}     {}: {}(), <{} {}>",
                    ref_num, caller, info.return_type, info.file, info.line
                );
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

    // Parse include options
    let include_data = args.include.iter().any(|s| s == "x");
    let include_underscore = args.include.iter().any(|s| s == "_");

    // Build call graph
    let mut graph = CallGraph::new(include_data, include_underscore);
    let mut streams = StreamTable::new();

    for file in &args.files {
        let ext = Path::new(file)
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("");

        match ext {
            "c" | "h" | "l" | "y" | "i" => {
                if let Err(e) = process_file(
                    file,
                    &mut streams,
                    &mut graph,
                    &args.defines,
                    &args.undefines,
                    &args.include_paths,
                ) {
                    eprintln!("cflow: {}: {}", file, e);
                }
            }
            "s" => {
                eprintln!("cflow: {}: assembly files not supported", file);
            }
            _ => {
                eprintln!("cflow: {}: unknown file type", file);
            }
        }
    }

    // Output flowgraph
    if args.reverse {
        print_reverse_flowgraph(&graph);
    } else {
        let roots = graph.get_roots();
        let mut ref_num: u32 = 0;
        let mut printed_refs = HashMap::new();

        for root in roots {
            let mut visited = HashSet::new();
            print_flowgraph(
                &graph,
                &root,
                0,
                args.depth,
                &mut visited,
                &mut ref_num,
                &mut printed_refs,
            );
        }
    }

    ExitCode::SUCCESS
}
