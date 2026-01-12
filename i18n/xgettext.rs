//
// Copyright (c) 2025 fox0
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;
use std::env::current_dir;
use std::ffi::OsStr;
use std::fs::{read_to_string, File};
use std::io::{self, BufReader, Read, Write};
use std::path::PathBuf;
use std::process::exit;

use clap::Parser;
#[cfg(debug_assertions)]
use gettextrs::bindtextdomain;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use posixutils_cc::parse::ast::{BlockItem, ExprKind, ExternalDecl, ForInit, Stmt};
use posixutils_cc::parse::Parser as CParser;
use posixutils_cc::strings::StringTable;
use posixutils_cc::symbol::SymbolTable;
use posixutils_cc::target::Target;
use posixutils_cc::token::{preprocess_with_defines, PreprocessConfig, StreamTable, Tokenizer};
use posixutils_cc::types::TypeTable;
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{parse_file, parse_str, LitStr};

#[derive(Parser)]
#[command(
    version,
    about = gettext("xgettext - extract gettext call strings from C-language source files (DEVELOPMENT)"),
    help_template = gettext("{about}\n\nUsage: {usage}\n\nArguments:\n{positionals}\n\nOptions:\n{options}"),
    disable_help_flag = true,
    disable_version_flag = true,
)]
struct Args {
    #[arg(
        short,
        help = gettext("Extract all strings, not just those found in calls to gettext family functions. Only one dot-po file shall be created")
    )]
    all: bool,

    #[arg(
        short,
        help = gettext("Name the default output file DEFAULT_DOMAIN.po instead of messages.po"),
        default_value = "messages"
    )]
    default_domain: String,

    #[arg(
        short,
        help = gettext("\
            Join messages from C-language source files with existing dot-po files. For each dot-po file that xgettext writes messages to, \
            if the file does not exist, it shall be created. New messages shall be appended but any subsections with duplicate msgid values \
            except the first (including msgid values found in an existing dot-po file) shall either be commented out or omitted \
            in the resulting dot-po file; if omitted, a warning message may be written to standard error. Domain directives in the existing \
            dot-po files shall be ignored; the assumption is that all previous msgid values belong to the same domain. The behavior \
            is unspecified if an existing dot-po file was not created by xgettext or has been modified by another application.")
    )]
    join: bool,

    #[arg(
        short = 'K',
        help = gettext("\
            Specify an additional keyword to be looked for:\n\
            * If KEYWORD_SPEC is an empty string, this shall disable the use of default keywords for the gettext family of functions.\n\
            * If KEYWORD_SPEC is a C identifier, xgettext shall look for strings in the first argument of each call to the function or macro KEYWORD_SPEC.\n\
            * If KEYWORD_SPEC is of the form id:argnum then xgettext shall treat the argnum-th argument of a call to the function or macro id as the msgid argument, \
            where argnum 1 is the first argument.\n\
            * If KEYWORD_SPEC is of the form id:argnum1,argnum2 then xgettext shall treat strings in the argnum1-th argument \
            and in the argnum2-th argument of a call to the function or macro id as the msgid and msgid_plural arguments, respectively."),
        default_value = "gettext"
    )]
    keyword_spec: Vec<String>,

    #[arg(
        short,
        help = gettext("Add comment lines to the output file indicating pathnames and line numbers in the source files where each extracted string is encountered")
    )]
    numbers_lines: bool,

    #[arg(
        short,
        help = gettext("Create output files in the directory specified by pathname instead of in the current working directory")
    )]
    pathname: Option<PathBuf>,

    #[arg(
        short = 'X',
        help = gettext("\
            Specify a file containing strings that shall not be extracted from the input files. \
            The format of EXCLUDE_FILE is identical to that of a dot-po file. However, \
            only statements containing msgid directives in EXCLUDE_FILE shall be used. All other statements shall be ignored.")
    )]
    exclude_file: Option<PathBuf>,

    #[arg(short, long, help = gettext("Print help"), action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(short = 'V', long, help = gettext("Print version"), action = clap::ArgAction::Version)]
    version: Option<bool>,

    #[arg(
        name = "FILE",
        trailing_var_arg = true,
        help = gettext("A pathname of an input file containing C-language source code. If '-' is specified for an instance of file, the standard input shall be used.")
    )]
    files: Vec<PathBuf>,
}

/// Parsed keyword specification for gettext functions
#[derive(Debug, Clone)]
pub struct KeywordSpec {
    /// Function/macro name to look for
    pub name: String,
    /// 1-indexed position of msgid argument
    pub msgid_arg: usize,
    /// 1-indexed position of msgid_plural argument (for ngettext-style)
    pub msgid_plural_arg: Option<usize>,
    /// 1-indexed position of msgctxt argument (for pgettext-style)
    pub msgctxt_arg: Option<usize>,
}

impl KeywordSpec {
    /// Parse a keyword spec string.
    /// Formats:
    /// - "id" -> msgid at arg 1
    /// - "id:N" -> msgid at arg N
    /// - "id:N,M" -> msgid at arg N, msgid_plural at arg M
    /// - "id:Nc,M" -> msgctxt at arg N, msgid at arg M
    /// - "id:Nc,M,P" -> msgctxt at arg N, msgid at arg M, msgid_plural at arg P
    pub fn parse(spec: &str) -> Option<Self> {
        if spec.is_empty() {
            return None;
        }

        let (name, args) = if let Some(idx) = spec.find(':') {
            (&spec[..idx], Some(&spec[idx + 1..]))
        } else {
            (spec, None)
        };

        if name.is_empty() {
            return None;
        }

        let (msgid_arg, msgid_plural_arg, msgctxt_arg) = if let Some(args) = args {
            Self::parse_args(args)?
        } else {
            (1, None, None)
        };

        Some(KeywordSpec {
            name: name.to_string(),
            msgid_arg,
            msgid_plural_arg,
            msgctxt_arg,
        })
    }

    /// Parse the argument specification after the colon
    fn parse_args(args: &str) -> Option<(usize, Option<usize>, Option<usize>)> {
        let parts: Vec<&str> = args.split(',').collect();

        match parts.len() {
            1 => {
                // "N" or "Nc"
                let (num, is_context) = Self::parse_arg_part(parts[0])?;
                if is_context {
                    // Just context specified, need at least msgid
                    None
                } else {
                    Some((num, None, None))
                }
            }
            2 => {
                // "N,M" or "Nc,M"
                let (num1, is_ctx1) = Self::parse_arg_part(parts[0])?;
                let (num2, is_ctx2) = Self::parse_arg_part(parts[1])?;
                if is_ctx2 {
                    // Second arg can't be context
                    None
                } else if is_ctx1 {
                    // Context, msgid
                    Some((num2, None, Some(num1)))
                } else {
                    // msgid, msgid_plural
                    Some((num1, Some(num2), None))
                }
            }
            3 => {
                // "Nc,M,P" -> context, msgid, msgid_plural
                let (num1, is_ctx1) = Self::parse_arg_part(parts[0])?;
                let (num2, is_ctx2) = Self::parse_arg_part(parts[1])?;
                let (num3, is_ctx3) = Self::parse_arg_part(parts[2])?;
                if !is_ctx1 || is_ctx2 || is_ctx3 {
                    None
                } else {
                    Some((num2, Some(num3), Some(num1)))
                }
            }
            _ => None,
        }
    }

    /// Parse a single argument part, returning (number, is_context)
    fn parse_arg_part(part: &str) -> Option<(usize, bool)> {
        let is_context = part.ends_with('c');
        let num_str = if is_context {
            &part[..part.len() - 1]
        } else {
            part
        };
        let num: usize = num_str.parse().ok()?;
        if num == 0 {
            return None;
        }
        Some((num, is_context))
    }

    /// Get default keyword specs for gettext family functions
    pub fn defaults() -> Vec<KeywordSpec> {
        vec![
            // Basic gettext
            KeywordSpec {
                name: "gettext".to_string(),
                msgid_arg: 1,
                msgid_plural_arg: None,
                msgctxt_arg: None,
            },
            // Plural forms
            KeywordSpec {
                name: "ngettext".to_string(),
                msgid_arg: 1,
                msgid_plural_arg: Some(2),
                msgctxt_arg: None,
            },
            // Context-aware
            KeywordSpec {
                name: "pgettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: None,
                msgctxt_arg: Some(1),
            },
            // Context-aware plural
            KeywordSpec {
                name: "npgettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: Some(3),
                msgctxt_arg: Some(1),
            },
            // Domain-specific variants
            KeywordSpec {
                name: "dgettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: None,
                msgctxt_arg: None,
            },
            KeywordSpec {
                name: "dngettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: Some(3),
                msgctxt_arg: None,
            },
            KeywordSpec {
                name: "dcgettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: None,
                msgctxt_arg: None,
            },
            KeywordSpec {
                name: "dcngettext".to_string(),
                msgid_arg: 2,
                msgid_plural_arg: Some(3),
                msgctxt_arg: None,
            },
        ]
    }
}

/// A message key combining context and msgid for deduplication
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MessageKey {
    pub msgctxt: Option<String>,
    pub msgid: String,
}

/// An extracted message with all its metadata
#[derive(Debug, Clone)]
pub struct Message {
    pub msgctxt: Option<String>,
    pub msgid: String,
    pub msgid_plural: Option<String>,
    pub locations: Vec<Line>,
}

impl Message {
    pub fn key(&self) -> MessageKey {
        MessageKey {
            msgctxt: self.msgctxt.clone(),
            msgid: self.msgid.clone(),
        }
    }
}

/// Source location of a message
#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct Line {
    pub path: String,
    pub line: usize,
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.path, self.line)
    }
}

/// Main message extractor
#[derive(Debug)]
pub struct Walker {
    keywords: HashMap<String, KeywordSpec>,
    numbers_lines: bool,
    extract_all: bool,
    exclude_msgids: Vec<String>,
    messages: HashMap<MessageKey, Message>,
}

impl Walker {
    /// Create a new Walker from keyword specs
    pub fn new(
        keyword_specs: Vec<String>,
        numbers_lines: bool,
        extract_all: bool,
        exclude_msgids: Vec<String>,
    ) -> Self {
        let mut keywords = HashMap::new();

        // Check if default keywords should be disabled (empty string in specs)
        let use_defaults = !keyword_specs.iter().any(|s| s.is_empty());

        if use_defaults {
            // Add default keywords
            for spec in KeywordSpec::defaults() {
                keywords.insert(spec.name.clone(), spec);
            }
        }

        // Parse and add user-specified keywords
        for spec_str in &keyword_specs {
            if spec_str.is_empty() {
                continue;
            }
            if let Some(spec) = KeywordSpec::parse(spec_str) {
                keywords.insert(spec.name.clone(), spec);
            }
        }

        Self {
            keywords,
            numbers_lines,
            extract_all,
            exclude_msgids,
            messages: HashMap::new(),
        }
    }

    /// Process a Rust source file
    pub fn process_rust_file(&mut self, content: String, path: String) -> Result<(), syn::Error> {
        let file = parse_file(&content)?;
        if self.extract_all {
            self.walk_all_strings(file.into_token_stream(), &path);
        } else {
            self.walk(file.into_token_stream(), &path);
        }
        Ok(())
    }

    /// Walk token stream looking for gettext function calls
    fn walk(&mut self, stream: TokenStream, path: &str) {
        let mut iter = stream.into_iter().peekable();
        while let Some(token) = iter.next() {
            match token {
                TokenTree::Group(group) => {
                    self.walk(group.stream(), path);
                }
                TokenTree::Ident(ident) => {
                    let name = ident.to_string();
                    if let Some(spec) = self.keywords.get(&name).cloned() {
                        if let Some(TokenTree::Group(group)) = iter.peek() {
                            if let Some(msg) = self.extract_message(&spec, group.stream(), path) {
                                self.push_message(msg);
                            }
                            let _ = iter.next();
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Walk token stream extracting all string literals (for -a flag)
    fn walk_all_strings(&mut self, stream: TokenStream, path: &str) {
        for token in stream {
            match token {
                TokenTree::Group(group) => {
                    self.walk_all_strings(group.stream(), path);
                }
                TokenTree::Literal(literal) => {
                    let span = literal.span();
                    if let Ok(lit_str) = parse_str::<LitStr>(&literal.to_string()) {
                        let value = lit_str.value();
                        if !self.exclude_msgids.contains(&value) {
                            let msg = Message {
                                msgctxt: None,
                                msgid: value,
                                msgid_plural: None,
                                locations: if self.numbers_lines {
                                    vec![Line {
                                        path: path.to_string(),
                                        line: span.start().line,
                                    }]
                                } else {
                                    vec![]
                                },
                            };
                            self.push_message(msg);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Extract arguments from a function call as a list of token streams
    fn extract_args(stream: TokenStream) -> Vec<(TokenStream, Span)> {
        let mut args = Vec::new();
        let mut current_arg = Vec::new();
        let mut first_span: Option<Span> = None;

        for token in stream {
            match &token {
                TokenTree::Punct(p) if p.as_char() == ',' => {
                    if !current_arg.is_empty() {
                        let stream: TokenStream = current_arg.drain(..).collect();
                        args.push((stream, first_span.unwrap_or_else(Span::call_site)));
                        first_span = None;
                    }
                }
                _ => {
                    if first_span.is_none() {
                        first_span = Some(token.span());
                    }
                    current_arg.push(token);
                }
            }
        }

        if !current_arg.is_empty() {
            let stream: TokenStream = current_arg.drain(..).collect();
            args.push((stream, first_span.unwrap_or_else(Span::call_site)));
        }

        args
    }

    /// Extract a string literal from a token stream (first literal only)
    fn extract_string(stream: &TokenStream) -> Option<(String, Span)> {
        for token in stream.clone() {
            if let TokenTree::Literal(literal) = token {
                let span = literal.span();
                if let Ok(lit_str) = parse_str::<LitStr>(&literal.to_string()) {
                    return Some((lit_str.value(), span));
                }
            }
        }
        None
    }

    /// Extract a message from a function call based on keyword spec
    fn extract_message(
        &self,
        spec: &KeywordSpec,
        stream: TokenStream,
        path: &str,
    ) -> Option<Message> {
        let args = Self::extract_args(stream);

        // Extract msgid (required)
        let msgid_idx = spec.msgid_arg.checked_sub(1)?;
        let (msgid_stream, msgid_span) = args.get(msgid_idx)?;
        let (msgid, _) = Self::extract_string(msgid_stream)?;

        // Check exclusion list
        if self.exclude_msgids.contains(&msgid) {
            return None;
        }

        // Extract msgid_plural (optional)
        let msgid_plural = if let Some(plural_arg) = spec.msgid_plural_arg {
            let plural_idx = plural_arg.checked_sub(1)?;
            args.get(plural_idx)
                .and_then(|(s, _)| Self::extract_string(s))
                .map(|(s, _)| s)
        } else {
            None
        };

        // Extract msgctxt (optional)
        let msgctxt = if let Some(ctx_arg) = spec.msgctxt_arg {
            let ctx_idx = ctx_arg.checked_sub(1)?;
            args.get(ctx_idx)
                .and_then(|(s, _)| Self::extract_string(s))
                .map(|(s, _)| s)
        } else {
            None
        };

        let locations = if self.numbers_lines {
            vec![Line {
                path: path.to_string(),
                line: msgid_span.start().line,
            }]
        } else {
            vec![]
        };

        Some(Message {
            msgctxt,
            msgid,
            msgid_plural,
            locations,
        })
    }

    /// Add a message to the collection, merging with existing if present
    fn push_message(&mut self, msg: Message) {
        let key = msg.key();
        if let Some(existing) = self.messages.get_mut(&key) {
            // Merge locations
            existing.locations.extend(msg.locations);
            // Prefer existing msgid_plural if set, otherwise use new one
            if existing.msgid_plural.is_none() {
                existing.msgid_plural = msg.msgid_plural;
            }
        } else {
            self.messages.insert(key, msg);
        }
    }

    /// Sort locations within each message
    pub fn sort(&mut self) {
        if !self.numbers_lines {
            return;
        }
        for msg in self.messages.values_mut() {
            msg.locations.sort();
        }
    }

    /// Escape a string for .pot file format
    fn escape(value: &str) -> String {
        format!(
            "\"{}\"",
            value
                .replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n\"\n\"")
        )
    }

    /// Process a C source file
    pub fn process_c_file(
        &mut self,
        content: &[u8],
        path: String,
        streams: &mut StreamTable,
    ) -> Result<(), String> {
        // Create stream
        let stream_id = streams.add(path.clone());

        // Create string table for identifier interning
        let mut strings = StringTable::new();

        // Tokenize
        let tokens = {
            let mut tokenizer = Tokenizer::new(content, stream_id, &mut strings);
            tokenizer.tokenize()
        };

        // Preprocess
        let target = Target::host();
        let preprocessed = preprocess_with_defines(
            tokens,
            &target,
            &mut strings,
            &path,
            &PreprocessConfig {
                defines: &[],
                undefines: &[],
                include_paths: &[],
                no_std_inc: true,
                no_builtin_inc: true,
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
                return Err(format!("Parse error in {}: {:?}", path, e));
            }
        };

        // Walk AST to extract gettext calls
        for item in &ast.items {
            match item {
                ExternalDecl::FunctionDef(func) => {
                    self.extract_from_c_stmt(&func.body, &strings, &symbols, streams, &path);
                }
                ExternalDecl::Declaration(decl) => {
                    for d in &decl.declarators {
                        if let Some(init) = &d.init {
                            self.extract_from_c_expr(init, &strings, &symbols, streams, &path);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Extract gettext calls from a C expression
    fn extract_from_c_expr(
        &mut self,
        expr: &posixutils_cc::parse::ast::Expr,
        strings: &StringTable,
        symbols: &SymbolTable,
        streams: &StreamTable,
        path: &str,
    ) {
        match &expr.kind {
            ExprKind::Call { func, args } => {
                // Check if this is a gettext-family function call
                if let ExprKind::Ident(symbol_id) = &func.kind {
                    let func_name = strings.get(symbols.get(*symbol_id).name).to_string();
                    if let Some(spec) = self.keywords.get(&func_name).cloned() {
                        // Extract message based on keyword spec
                        if let Some(msg) = self.extract_c_message(
                            &spec,
                            args,
                            strings,
                            streams,
                            path,
                            expr.pos.line,
                        ) {
                            self.push_message(msg);
                        }
                    }
                }
                // Recurse into arguments
                for arg in args {
                    self.extract_from_c_expr(arg, strings, symbols, streams, path);
                }
            }
            ExprKind::StringLit(s) => {
                // Handle -a flag (extract all strings)
                if self.extract_all && !self.exclude_msgids.contains(s) {
                    let msg = Message {
                        msgctxt: None,
                        msgid: s.clone(),
                        msgid_plural: None,
                        locations: if self.numbers_lines {
                            vec![Line {
                                path: path.to_string(),
                                line: expr.pos.line as usize,
                            }]
                        } else {
                            vec![]
                        },
                    };
                    self.push_message(msg);
                }
            }
            ExprKind::Unary { operand, .. } => {
                self.extract_from_c_expr(operand, strings, symbols, streams, path);
            }
            ExprKind::Binary { left, right, .. } => {
                self.extract_from_c_expr(left, strings, symbols, streams, path);
                self.extract_from_c_expr(right, strings, symbols, streams, path);
            }
            ExprKind::Assign { target, value, .. } => {
                self.extract_from_c_expr(target, strings, symbols, streams, path);
                self.extract_from_c_expr(value, strings, symbols, streams, path);
            }
            ExprKind::PostInc(e) | ExprKind::PostDec(e) => {
                self.extract_from_c_expr(e, strings, symbols, streams, path);
            }
            ExprKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                self.extract_from_c_expr(cond, strings, symbols, streams, path);
                self.extract_from_c_expr(then_expr, strings, symbols, streams, path);
                self.extract_from_c_expr(else_expr, strings, symbols, streams, path);
            }
            ExprKind::Member { expr, .. } | ExprKind::Arrow { expr, .. } => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
            }
            ExprKind::Index { array, index } => {
                self.extract_from_c_expr(array, strings, symbols, streams, path);
                self.extract_from_c_expr(index, strings, symbols, streams, path);
            }
            ExprKind::Cast { expr, .. } => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
            }
            ExprKind::SizeofExpr(e) => {
                self.extract_from_c_expr(e, strings, symbols, streams, path);
            }
            ExprKind::Comma(exprs) => {
                for e in exprs {
                    self.extract_from_c_expr(e, strings, symbols, streams, path);
                }
            }
            ExprKind::InitList { elements } => {
                for elem in elements {
                    self.extract_from_c_expr(&elem.value, strings, symbols, streams, path);
                }
            }
            ExprKind::CompoundLiteral { elements, .. } => {
                for elem in elements {
                    self.extract_from_c_expr(&elem.value, strings, symbols, streams, path);
                }
            }
            _ => {}
        }
    }

    /// Extract gettext calls from a C statement
    fn extract_from_c_stmt(
        &mut self,
        stmt: &Stmt,
        strings: &StringTable,
        symbols: &SymbolTable,
        streams: &StreamTable,
        path: &str,
    ) {
        match stmt {
            Stmt::Empty => {}
            Stmt::Expr(expr) => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
            }
            Stmt::Block(items) => {
                for item in items {
                    match item {
                        BlockItem::Statement(s) => {
                            self.extract_from_c_stmt(s, strings, symbols, streams, path);
                        }
                        BlockItem::Declaration(decl) => {
                            for d in &decl.declarators {
                                if let Some(init) = &d.init {
                                    self.extract_from_c_expr(init, strings, symbols, streams, path);
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
                self.extract_from_c_expr(cond, strings, symbols, streams, path);
                self.extract_from_c_stmt(then_stmt, strings, symbols, streams, path);
                if let Some(else_s) = else_stmt {
                    self.extract_from_c_stmt(else_s, strings, symbols, streams, path);
                }
            }
            Stmt::While { cond, body } => {
                self.extract_from_c_expr(cond, strings, symbols, streams, path);
                self.extract_from_c_stmt(body, strings, symbols, streams, path);
            }
            Stmt::DoWhile { body, cond } => {
                self.extract_from_c_stmt(body, strings, symbols, streams, path);
                self.extract_from_c_expr(cond, strings, symbols, streams, path);
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(i) = init {
                    match i {
                        ForInit::Expression(e) => {
                            self.extract_from_c_expr(e, strings, symbols, streams, path);
                        }
                        ForInit::Declaration(d) => {
                            for decl in &d.declarators {
                                if let Some(init_expr) = &decl.init {
                                    self.extract_from_c_expr(
                                        init_expr, strings, symbols, streams, path,
                                    );
                                }
                            }
                        }
                    }
                }
                if let Some(c) = cond {
                    self.extract_from_c_expr(c, strings, symbols, streams, path);
                }
                if let Some(p) = post {
                    self.extract_from_c_expr(p, strings, symbols, streams, path);
                }
                self.extract_from_c_stmt(body, strings, symbols, streams, path);
            }
            Stmt::Switch { expr, body } => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
                self.extract_from_c_stmt(body, strings, symbols, streams, path);
            }
            Stmt::Case(expr) => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
            }
            Stmt::Default => {}
            Stmt::Return(Some(expr)) => {
                self.extract_from_c_expr(expr, strings, symbols, streams, path);
            }
            Stmt::Label { stmt, .. } => {
                self.extract_from_c_stmt(stmt, strings, symbols, streams, path);
            }
            _ => {}
        }
    }

    /// Extract a message from C function call arguments based on keyword spec
    fn extract_c_message(
        &self,
        spec: &KeywordSpec,
        args: &[posixutils_cc::parse::ast::Expr],
        _strings: &StringTable,
        _streams: &StreamTable,
        path: &str,
        line: u32,
    ) -> Option<Message> {
        // Extract msgid (required)
        let msgid_idx = spec.msgid_arg.checked_sub(1)?;
        let msgid = Self::extract_c_string(&args.get(msgid_idx)?.kind)?;

        // Check exclusion list
        if self.exclude_msgids.contains(&msgid) {
            return None;
        }

        // Extract msgid_plural (optional)
        let msgid_plural = if let Some(plural_arg) = spec.msgid_plural_arg {
            let plural_idx = plural_arg.checked_sub(1)?;
            args.get(plural_idx)
                .and_then(|e| Self::extract_c_string(&e.kind))
        } else {
            None
        };

        // Extract msgctxt (optional)
        let msgctxt = if let Some(ctx_arg) = spec.msgctxt_arg {
            let ctx_idx = ctx_arg.checked_sub(1)?;
            args.get(ctx_idx)
                .and_then(|e| Self::extract_c_string(&e.kind))
        } else {
            None
        };

        let locations = if self.numbers_lines {
            vec![Line {
                path: path.to_string(),
                line: line as usize,
            }]
        } else {
            vec![]
        };

        Some(Message {
            msgctxt,
            msgid,
            msgid_plural,
            locations,
        })
    }

    /// Extract string literal from a C expression kind
    fn extract_c_string(kind: &ExprKind) -> Option<String> {
        match kind {
            ExprKind::StringLit(s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl std::fmt::Display for Walker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut vec: Vec<_> = self.messages.iter().collect();
        vec.sort_by(|a, b| a.0.cmp(b.0));
        for (_, msg) in vec {
            // Write location comments
            for line in &msg.locations {
                writeln!(f, "#: {}", line)?;
            }

            // Write msgctxt if present
            if let Some(ref ctx) = msg.msgctxt {
                writeln!(f, "msgctxt {}", Self::escape(ctx))?;
            }

            // Write msgid
            writeln!(f, "msgid {}", Self::escape(&msg.msgid))?;

            // Write msgid_plural and msgstr[n] if plural form
            if let Some(ref plural) = msg.msgid_plural {
                writeln!(f, "msgid_plural {}", Self::escape(plural))?;
                writeln!(f, "msgstr[0] \"\"")?;
                writeln!(f, "msgstr[1] \"\"")?;
            } else {
                writeln!(f, "msgstr \"\"")?;
            }

            writeln!(f)?;
        }
        Ok(())
    }
}

/// Unescape a PO/POT file string value.
/// Handles escape sequences in the correct order to avoid misinterpretation.
fn unescape_pot_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some('\\') => {
                    result.push('\\');
                    chars.next();
                }
                Some('"') => {
                    result.push('"');
                    chars.next();
                }
                Some('n') => {
                    result.push('\n');
                    chars.next();
                }
                Some('t') => {
                    result.push('\t');
                    chars.next();
                }
                Some('r') => {
                    result.push('\r');
                    chars.next();
                }
                _ => {
                    // Unknown escape sequence, keep as-is
                    result.push(c);
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a .pot file and extract msgid strings for exclusion
fn parse_exclude_file(path: &PathBuf) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let content = read_to_string(path)?;
    let mut msgids = Vec::new();

    for line in content.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("msgid ") {
            // Extract the string value
            let value = rest.trim();
            if value.starts_with('"') && value.ends_with('"') && value.len() >= 2 {
                let unescaped = unescape_pot_string(&value[1..value.len() - 1]);
                if !unescaped.is_empty() {
                    msgids.push(unescaped);
                }
            }
        }
    }

    Ok(msgids)
}

/// Parse existing .pot file for -j (join) option
fn parse_existing_pot(path: &PathBuf) -> Result<String, Box<dyn std::error::Error>> {
    if path.exists() {
        Ok(read_to_string(path)?)
    } else {
        Ok(String::new())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    #[cfg(debug_assertions)]
    bindtextdomain("posixutils-rs", "locale")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    if args.files.is_empty() {
        eprintln!("xgettext: {}", gettext("no input file given"));
        exit(1);
    }

    // Parse exclude file if specified
    let exclude_msgids = if let Some(ref exclude_path) = args.exclude_file {
        parse_exclude_file(exclude_path)?
    } else {
        Vec::new()
    };

    let mut walker = Walker::new(
        args.keyword_spec,
        args.numbers_lines,
        args.all,
        exclude_msgids,
    );

    // Stream table for C file processing
    let mut streams = StreamTable::new();

    for path in &args.files {
        let path_str = path.to_string_lossy().to_string();

        // Handle stdin
        if path_str == "-" {
            let mut content = Vec::new();
            io::stdin().read_to_end(&mut content)?;
            // Default to C for stdin (POSIX spec)
            if let Err(e) = walker.process_c_file(&content, "<stdin>".to_string(), &mut streams) {
                eprintln!("xgettext: {}", e);
                exit(1);
            }
            continue;
        }

        match path.extension().and_then(OsStr::to_str) {
            Some("rs") => {
                let content = read_to_string(path)?;
                walker.process_rust_file(content, path_str)?;
            }
            Some("c") | Some("h") => {
                let file = File::open(path)?;
                let mut reader = BufReader::new(file);
                let mut content = Vec::new();
                reader.read_to_end(&mut content)?;
                if let Err(e) = walker.process_c_file(&content, path_str, &mut streams) {
                    eprintln!("xgettext: {}", e);
                    exit(1);
                }
            }
            _ => {
                eprintln!(
                    "xgettext: {}: {}",
                    path_str,
                    gettext("unsupported file type")
                );
                exit(1);
            }
        }
    }

    walker.sort();

    let output_path = args
        .pathname
        .unwrap_or_else(|| current_dir().unwrap())
        .join(format!("{}.pot", args.default_domain));

    // Handle -j (join) option
    let existing_content = if args.join {
        parse_existing_pot(&output_path)?
    } else {
        String::new()
    };

    let new_content = format!("{}", walker);

    let final_content = if args.join && !existing_content.is_empty() {
        // Simple join: prepend existing content
        // A more sophisticated implementation would merge and deduplicate
        format!("{}{}", existing_content, new_content)
    } else {
        new_content
    };

    let mut output = File::create(output_path)?;
    write!(output, "{}", final_content)?;

    exit(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    // KeywordSpec parsing tests
    #[test]
    fn test_keyword_spec_simple() {
        let spec = KeywordSpec::parse("gettext").unwrap();
        assert_eq!(spec.name, "gettext");
        assert_eq!(spec.msgid_arg, 1);
        assert_eq!(spec.msgid_plural_arg, None);
        assert_eq!(spec.msgctxt_arg, None);
    }

    #[test]
    fn test_keyword_spec_argnum() {
        let spec = KeywordSpec::parse("dgettext:2").unwrap();
        assert_eq!(spec.name, "dgettext");
        assert_eq!(spec.msgid_arg, 2);
        assert_eq!(spec.msgid_plural_arg, None);
        assert_eq!(spec.msgctxt_arg, None);
    }

    #[test]
    fn test_keyword_spec_plural() {
        let spec = KeywordSpec::parse("ngettext:1,2").unwrap();
        assert_eq!(spec.name, "ngettext");
        assert_eq!(spec.msgid_arg, 1);
        assert_eq!(spec.msgid_plural_arg, Some(2));
        assert_eq!(spec.msgctxt_arg, None);
    }

    #[test]
    fn test_keyword_spec_context() {
        let spec = KeywordSpec::parse("pgettext:1c,2").unwrap();
        assert_eq!(spec.name, "pgettext");
        assert_eq!(spec.msgid_arg, 2);
        assert_eq!(spec.msgid_plural_arg, None);
        assert_eq!(spec.msgctxt_arg, Some(1));
    }

    #[test]
    fn test_keyword_spec_context_plural() {
        let spec = KeywordSpec::parse("npgettext:1c,2,3").unwrap();
        assert_eq!(spec.name, "npgettext");
        assert_eq!(spec.msgid_arg, 2);
        assert_eq!(spec.msgid_plural_arg, Some(3));
        assert_eq!(spec.msgctxt_arg, Some(1));
    }

    #[test]
    fn test_keyword_spec_empty() {
        assert!(KeywordSpec::parse("").is_none());
    }

    #[test]
    fn test_keyword_spec_invalid() {
        assert!(KeywordSpec::parse(":1").is_none());
        assert!(KeywordSpec::parse("func:0").is_none());
        assert!(KeywordSpec::parse("func:1c").is_none()); // context alone is invalid
    }

    // Walker tests
    #[test]
    fn test_process_rust_file() {
        let code = String::from(
            r#"fn main() {
    assert_eq!("Hello, world!", gettext("Hello, world!"));
}
"#,
        );
        let mut walker = Walker::new(vec!["gettext".into()], true, false, vec![]);
        walker
            .process_rust_file(code, "test_process_rust_file.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 1);
        let key = MessageKey {
            msgctxt: None,
            msgid: "Hello, world!".to_string(),
        };
        let msg = &walker.messages[&key];
        assert_eq!(msg.locations.len(), 1);
        assert_eq!(
            msg.locations[0],
            Line {
                path: "test_process_rust_file.rs".into(),
                line: 2
            }
        );
    }

    #[test]
    fn test_process_rust_file_format() {
        let code = String::from(
            r#"fn main() {
    assert_eq!("Hello, world!", gettext("Hello, world!"));
}
"#,
        );
        let mut walker = Walker::new(vec!["gettext".into()], true, false, vec![]);
        walker
            .process_rust_file(code, "test_process_rust_file_format.rs".to_string())
            .unwrap();
        assert_eq!(
            format!("{}", walker),
            r#"#: test_process_rust_file_format.rs:2
msgid "Hello, world!"
msgstr ""

"#
        );
    }

    #[test]
    fn test_ngettext_extraction() {
        let code = String::from(
            r#"fn main() {
    println!("{}", ngettext("One thing", "Multiple things", n));
}
"#,
        );
        let mut walker = Walker::new(vec!["ngettext:1,2".into()], true, false, vec![]);
        walker
            .process_rust_file(code, "test.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 1);
        let key = MessageKey {
            msgctxt: None,
            msgid: "One thing".to_string(),
        };
        let msg = &walker.messages[&key];
        assert_eq!(msg.msgid_plural, Some("Multiple things".to_string()));
    }

    #[test]
    fn test_pgettext_extraction() {
        let code = String::from(
            r#"fn main() {
    println!("{}", pgettext("Context", "Message"));
}
"#,
        );
        let mut walker = Walker::new(vec!["pgettext:1c,2".into()], true, false, vec![]);
        walker
            .process_rust_file(code, "test.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 1);
        let key = MessageKey {
            msgctxt: Some("Context".to_string()),
            msgid: "Message".to_string(),
        };
        assert!(walker.messages.contains_key(&key));
    }

    #[test]
    fn test_npgettext_extraction() {
        let code = String::from(
            r#"fn main() {
    println!("{}", npgettext("Context", "One", "Many", n));
}
"#,
        );
        let mut walker = Walker::new(vec!["npgettext:1c,2,3".into()], true, false, vec![]);
        walker
            .process_rust_file(code, "test.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 1);
        let key = MessageKey {
            msgctxt: Some("Context".to_string()),
            msgid: "One".to_string(),
        };
        let msg = &walker.messages[&key];
        assert_eq!(msg.msgid_plural, Some("Many".to_string()));
    }

    #[test]
    fn test_escape() {
        let value = Walker::escape(
            "{about}\n\nUsage: {usage}\n\nArguments:\n{positionals}\n\nOptions:\n{options}",
        );
        assert_eq!(
            value,
            r#""{about}\n"
"\n"
"Usage: {usage}\n"
"\n"
"Arguments:\n"
"{positionals}\n"
"\n"
"Options:\n"
"{options}""#
        );
    }

    #[test]
    fn test_escape2() {
        let value = Walker::escape("string \"string2\" string");
        assert_eq!(value, r#""string \"string2\" string""#);
    }

    #[test]
    fn test_escape_backslash() {
        let value = Walker::escape("path\\to\\file");
        assert_eq!(value, r#""path\\to\\file""#);
    }

    #[test]
    fn test_extract_all_strings() {
        let code = String::from(
            r#"fn main() {
    let s = "string1";
    let t = "string2";
}
"#,
        );
        let mut walker = Walker::new(vec!["gettext".into()], true, true, vec![]);
        walker
            .process_rust_file(code, "test.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 2);
    }

    #[test]
    fn test_exclude_msgids() {
        let code = String::from(
            r#"fn main() {
    gettext("keep");
    gettext("exclude");
}
"#,
        );
        let mut walker = Walker::new(
            vec!["gettext".into()],
            true,
            false,
            vec!["exclude".to_string()],
        );
        walker
            .process_rust_file(code, "test.rs".to_string())
            .unwrap();
        assert_eq!(walker.messages.len(), 1);
        let key = MessageKey {
            msgctxt: None,
            msgid: "keep".to_string(),
        };
        assert!(walker.messages.contains_key(&key));
    }
}
