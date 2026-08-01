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
use gettextrs::gettext;
use posixutils_cc::parse::ast::{ExprKind, ExternalDecl, Stmt};
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::ppargs;
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

    /// Depth cutoff for the flowgraph.
    ///
    /// Signed, and hyphen values are allowed, so that a negative argument can be
    /// recognized and ignored rather than rejected: POSIX says "attempts to set
    /// the cut-off depth to a non-positive integer shall be ignored".
    #[arg(short = 'd', long, allow_hyphen_values = true,
          help = gettext("Cut off flowgraph at specified depth"))]
    depth: Option<i64>,

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

/// What kind of global a graph node describes.
///
/// POSIX STDOUT formats the two differently: a function's definition is written
/// as an abstract declaration ending in `()`, e.g. `f: int(), <file.c 13>`,
/// whereas a data symbol has no parameter list, e.g. `i: int, <file.c 1>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeKind {
    Function,
    Data,
}

/// Information about a global definition
#[derive(Debug, Clone)]
struct FunctionInfo {
    /// Symbol name
    name: String,
    /// Type as string
    return_type: String,
    /// Source file
    file: String,
    /// Line number
    line: u32,
    /// Globals referenced by this one (callees, plus data symbols under -i x)
    calls: Vec<String>,
    /// Whether this is a function or a data object
    kind: NodeKind,
}

impl FunctionInfo {
    /// The `<definition>` field of the POSIX output format.
    fn definition(&self) -> String {
        match self.kind {
            NodeKind::Function => {
                format!("{}(), <{} {}>", self.return_type, self.file, self.line)
            }
            NodeKind::Data => format!("{}, <{} {}>", self.return_type, self.file, self.line),
        }
    }
}

/// Global call graph
struct CallGraph {
    /// Definitions: name -> info
    functions: HashMap<String, FunctionInfo>,
    /// All known names (defined or referenced)
    all_names: HashSet<String>,
    /// Include external and static data symbols (-i x)
    include_data: bool,
    /// Include underscore names
    include_underscore: bool,
}

impl CallGraph {
    fn new(include_data: bool, include_underscore: bool) -> Self {
        Self {
            functions: HashMap::new(),
            all_names: HashSet::new(),
            include_data,
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

    /// Get root functions (not called by any other function).
    ///
    /// Data symbols are never roots: they are leaves of the flowgraph, reached
    /// through the functions that reference them.
    fn get_roots(&self) -> Vec<String> {
        let called: HashSet<_> = self
            .functions
            .values()
            .flat_map(|f| f.calls.iter())
            .cloned()
            .collect();

        let mut roots: Vec<_> = self
            .functions
            .values()
            .filter(|info| info.kind == NodeKind::Function && !called.contains(&info.name))
            .map(|info| info.name.clone())
            .collect();
        roots.sort();
        roots
    }

    /// Get callers of a function (for reverse mode).
    ///
    /// POSIX ENVIRONMENT VARIABLES: LC_COLLATE determines the ordering of the
    /// output when -r is used.
    fn get_callers(&self, name: &str) -> Vec<String> {
        let mut callers: Vec<_> = self
            .functions
            .iter()
            .filter(|(_, info)| info.calls.contains(&name.to_string()))
            .map(|(caller, _)| caller.clone())
            .collect();
        callers.sort_by(|a, b| plib::locale::strcoll(a, b));
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
        ExprKind::SizeofExpr(e) | ExprKind::AlignofExpr(e) => {
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
        | ExprKind::Alloca { size: arg }
        | ExprKind::FrameAddress { level: arg }
        | ExprKind::ReturnAddress { level: arg } => {
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

/// Collect identifier references appearing anywhere in a statement.
///
/// Used for `-i x`: a data symbol is reached through an ordinary identifier
/// reference, not a call, so the call-collecting walk above never sees it.
fn extract_idents_from_stmt(
    stmt: &Stmt,
    strings: &StringTable,
    symbols: &SymbolTable,
    out: &mut Vec<String>,
) {
    // Reuse the call walker's traversal by collecting from every expression it
    // would visit; simplest correct approach is a dedicated recursive walk.
    fn from_expr(
        expr: &posixutils_cc::parse::ast::Expr,
        strings: &StringTable,
        symbols: &SymbolTable,
        out: &mut Vec<String>,
    ) {
        if let ExprKind::Ident(symbol_id) = &expr.kind {
            let name = strings.get(symbols.get(*symbol_id).name).to_string();
            if !out.contains(&name) {
                out.push(name);
            }
        }
        visit_subexprs(expr, &mut |e| from_expr(e, strings, symbols, out));
    }

    visit_stmt_exprs(stmt, &mut |e| from_expr(e, strings, symbols, out));
}

/// Apply `f` to every immediate sub-expression of `expr`.
fn visit_subexprs(
    expr: &posixutils_cc::parse::ast::Expr,
    f: &mut dyn FnMut(&posixutils_cc::parse::ast::Expr),
) {
    match &expr.kind {
        ExprKind::Call { func, args } => {
            f(func);
            for a in args {
                f(a);
            }
        }
        ExprKind::Unary { operand, .. } => f(operand),
        ExprKind::Binary { left, right, .. } => {
            f(left);
            f(right);
        }
        ExprKind::Assign { target, value, .. } => {
            f(target);
            f(value);
        }
        ExprKind::PostInc(e) | ExprKind::PostDec(e) => f(e),
        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            f(cond);
            f(then_expr);
            f(else_expr);
        }
        ExprKind::Member { expr, .. } | ExprKind::Arrow { expr, .. } => f(expr),
        ExprKind::Index { array, index } => {
            f(array);
            f(index);
        }
        ExprKind::Cast { expr, .. } => f(expr),
        ExprKind::SizeofExpr(e) | ExprKind::AlignofExpr(e) => f(e),
        ExprKind::Comma(exprs) => {
            for e in exprs {
                f(e);
            }
        }
        ExprKind::InitList { elements } | ExprKind::CompoundLiteral { elements, .. } => {
            for elem in elements {
                f(&elem.value);
            }
        }
        _ => {}
    }
}

/// Apply `f` to every expression directly contained in `stmt` (recursively).
fn visit_stmt_exprs(stmt: &Stmt, f: &mut dyn FnMut(&posixutils_cc::parse::ast::Expr)) {
    match stmt {
        Stmt::Expr(e) => f(e),
        Stmt::Block(items) => {
            for item in items {
                match item {
                    posixutils_cc::parse::ast::BlockItem::Statement(s) => visit_stmt_exprs(s, f),
                    posixutils_cc::parse::ast::BlockItem::Declaration(d) => {
                        for decl in &d.declarators {
                            if let Some(init) = &decl.init {
                                f(init);
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
            f(cond);
            visit_stmt_exprs(then_stmt, f);
            if let Some(e) = else_stmt {
                visit_stmt_exprs(e, f);
            }
        }
        Stmt::While { cond, body } => {
            f(cond);
            visit_stmt_exprs(body, f);
        }
        Stmt::DoWhile { body, cond } => {
            visit_stmt_exprs(body, f);
            f(cond);
        }
        Stmt::For {
            init,
            cond,
            post,
            body,
        } => {
            if let Some(i) = init {
                match i {
                    posixutils_cc::parse::ast::ForInit::Expression(e) => f(e),
                    posixutils_cc::parse::ast::ForInit::Declaration(d) => {
                        for decl in &d.declarators {
                            if let Some(init_expr) = &decl.init {
                                f(init_expr);
                            }
                        }
                    }
                }
            }
            if let Some(c) = cond {
                f(c);
            }
            if let Some(p) = post {
                f(p);
            }
            visit_stmt_exprs(body, f);
        }
        Stmt::Return(Some(e)) => f(e),
        Stmt::Switch { expr, body } => {
            f(expr);
            visit_stmt_exprs(body, f);
        }
        Stmt::Case(e) => f(e),
        Stmt::Label { stmt, .. } => visit_stmt_exprs(stmt, f),
        _ => {}
    }
}

/// Resolve the line on which a definition's *name* appears.
///
/// `FunctionDef::pos` points at the start of the external declaration, which is
/// the type-specifier line. When the return type sits on its own line — the
/// style used by the spec's own EXAMPLE — that is one line above the declarator.
/// POSIX shows the declarator's line, so scan forward from the declaration start
/// for the first line on which the name occurs as an identifier.
fn declarator_line(lines: &[&str], start_line: u32, name: &str) -> u32 {
    if start_line == 0 {
        return start_line;
    }
    let first = (start_line as usize).saturating_sub(1);
    // A declarator cannot be far from its declaration start; bound the scan so a
    // stray later occurrence of the name can never be picked up.
    for (offset, line) in lines.iter().skip(first).take(8).enumerate() {
        let code = match line.find("//") {
            Some(i) => &line[..i],
            None => line,
        };
        let hit = code
            .split(|c: char| !(c.is_alphanumeric() || c == '_'))
            .any(|t| t == name);
        if hit {
            return start_line + offset as u32;
        }
    }
    start_line
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
        TypeKind::Int128 => {
            if is_unsigned {
                "__uint128_t".to_string()
            } else {
                "__int128".to_string()
            }
        }
        TypeKind::Float => "float".to_string(),
        TypeKind::Double => "double".to_string(),
        TypeKind::LongDouble => "long double".to_string(),
        TypeKind::Float16 => "_Float16".to_string(),
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

    // Preprocess.
    //
    // A .i operand is already the output of `c17 -E`; POSIX (via c17's OPERANDS
    // section) says "the processing already performed by c17 -E when the file
    // was produced shall not be repeated", so it goes straight to the parser.
    let target = Target::host();
    let already_preprocessed = Path::new(path)
        .extension()
        .and_then(|s| s.to_str())
        .map(|e| e == "i")
        .unwrap_or(false);
    let preprocessed = if already_preprocessed {
        tokens
    } else {
        preprocess_with_defines(
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
        )
    };

    // Create symbol table and type table
    let mut symbols = SymbolTable::new();
    let mut types = TypeTable::new(&target);

    // Parse
    let mut parser = CParser::new(&preprocessed, &strings, &mut symbols, &mut types);
    let ast = match parser.parse_translation_unit() {
        Ok(ast) => ast,
        Err(e) => {
            plib::diag::error(&format!("{}: {}: {}", path, gettext("parse error"), e));
            return Ok(());
        }
    };

    // Source text, used to resolve declarator lines (see `declarator_line`).
    let text = String::from_utf8_lossy(&buffer);
    let lines: Vec<&str> = text.lines().collect();

    // Pass 1: file-scope data objects. Needed both as graph nodes (under -i x)
    // and as the set that tells pass 2 which identifier references are data.
    let mut data_symbols: HashMap<String, FunctionInfo> = HashMap::new();
    for item in &ast.items {
        if let ExternalDecl::Declaration(decl) = item {
            for d in &decl.declarators {
                let sym = symbols.get(d.symbol);
                if sym.is_typedef() {
                    continue;
                }
                let typ = types.get(d.typ);
                // Functions declared (not defined) are not data.
                if typ.kind == TypeKind::Function {
                    continue;
                }
                let name = strings.get(sym.name).to_string();
                let line = declarator_line(&lines, d.pos.line, &name);
                data_symbols.insert(
                    name.clone(),
                    FunctionInfo {
                        name,
                        return_type: format_type(d.typ, &types),
                        file: path.to_string(),
                        line,
                        calls: Vec::new(),
                        kind: NodeKind::Data,
                    },
                );
            }
        }
    }

    // Pass 2: function definitions and what they reference.
    for item in &ast.items {
        if let ExternalDecl::FunctionDef(func) = item {
            let name = strings.get(func.name).to_string();
            let return_type = format_type(func.return_type, &types);
            let line = declarator_line(&lines, func.pos.line, &name);

            let mut calls = Vec::new();
            extract_calls_from_stmt(&func.body, &strings, &symbols, &mut calls);

            // Data references come after calls, matching the ordering of the
            // spec's worked EXAMPLE (callee `h` before data `i`).
            if graph.include_data {
                let mut idents = Vec::new();
                extract_idents_from_stmt(&func.body, &strings, &symbols, &mut idents);
                for ident in idents {
                    if data_symbols.contains_key(&ident) && !calls.contains(&ident) {
                        calls.push(ident);
                    }
                }
            }

            // Filter calls based on include options (e.g., underscore names)
            calls.retain(|c| graph.should_include(c));

            let info = FunctionInfo {
                name,
                return_type,
                file: path.to_string(),
                line,
                calls,
                kind: NodeKind::Function,
            };

            graph.add_function(info);
        }
    }

    // Register the data symbols themselves so their definitions can be printed.
    if graph.include_data {
        for info in data_symbols.into_values() {
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
        // Already printed: POSIX STDOUT says "subsequent references to that name
        // contain only the reference number of the line where the definition can
        // be found", in the same "%d %s:%s" shape as every other line.
        if let Some(prev_ref) = printed_refs.get(name) {
            println!("{}{} {}: {}", my_ref, indent, name, prev_ref);
            return;
        }

        printed_refs.insert(name.to_string(), my_ref);

        // Print the definition
        println!("{}{} {}: {}", my_ref, indent, name, info.definition());

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

/// Print reverse flowgraph (callers of each function).
///
/// POSIX -r: "The listing shall also be sorted in lexicographical order by"
/// name, and LC_COLLATE determines that ordering.
fn print_reverse_flowgraph(graph: &CallGraph) {
    let mut all_names: Vec<_> = graph.all_names.iter().cloned().collect();
    all_names.sort_by(|a, b| plib::locale::strcoll(a, b));

    let mut ref_num: u32 = 0;

    for name in all_names {
        ref_num += 1;

        match graph.functions.get(&name) {
            Some(info) => println!("{} {}: {}", ref_num, name, info.definition()),
            None => println!("{} {}: <>", ref_num, name),
        }

        // Print callers
        for caller in graph.get_callers(&name) {
            ref_num += 1;
            if let Some(info) = graph.functions.get(&caller) {
                println!("{}     {}: {}", ref_num, caller, info.definition());
            }
        }
    }
}

// ============================================================================
// Main
// ============================================================================

fn main() -> ExitCode {
    plib::diag::init_locale("cflow");

    let args = Args::parse();

    // -D/-U are order-significant for this utility (unlike c17, where -U
    // always wins). clap collects each flag into its own list, losing the
    // interleaving, so rescan the raw argument vector and replay it.
    let (defines, undefines) = ppargs::resolve(&ppargs::scan(std::env::args()));

    // Parse include options. POSIX defines exactly two values for -i.
    for incl in &args.include {
        if incl != "x" && incl != "_" {
            plib::diag::error(&format!(
                "{}: {}",
                incl,
                gettext("invalid -i argument (expected 'x' or '_')")
            ));
            return ExitCode::from(1);
        }
    }
    let include_data = args.include.iter().any(|s| s == "x");
    let include_underscore = args.include.iter().any(|s| s == "_");

    // "Attempts to set the cut-off depth to a non-positive integer shall be
    // ignored" -- i.e. behave as if -d had not been given at all.
    let max_depth: Option<usize> = args.depth.filter(|d| *d > 0).map(|d| d as usize);

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
                    &defines,
                    &undefines,
                    &args.include_paths,
                ) {
                    plib::diag::error(&format!("{}: {}", file, e));
                }
            }
            "s" => {
                plib::diag::error(&format!(
                    "{}: {}",
                    file,
                    gettext("assembly files not supported")
                ));
            }
            _ => {
                plib::diag::error(&format!("{}: {}", file, gettext("unknown file type")));
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
                max_depth,
                &mut visited,
                &mut ref_num,
                &mut printed_refs,
            );
        }
    }

    exit_code()
}

/// Combine this utility's own diagnostics with any emitted by the C front end
/// (undeclared identifiers and friends are reported by `posixutils_cc::diag`,
/// which keeps its own counter). POSIX requires a non-zero status when either
/// has fired.
fn exit_code() -> ExitCode {
    if plib::diag::has_errors() || posixutils_cc::diag::has_error() != 0 {
        ExitCode::from(1)
    } else {
        ExitCode::SUCCESS
    }
}
