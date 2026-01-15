//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C Preprocessor
// Implements C99 preprocessing directives and macro expansion
//
// Main API: preprocess(tokens, target, idents) -> preprocessed_tokens
//

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use super::lexer::{IdentTable, Position, SpecialToken, Token, TokenType, TokenValue, Tokenizer};
use crate::arch;
use crate::builtin_headers;
use crate::diag;
use crate::os;
use crate::target::Target;

const DEFAULT_MACRO_CAPACITY: usize = 32;
const DEFAULT_COND_STACK_CAPACITY: usize = 8;
const DEFAULT_INCLUDE_PATH_CAPACITY: usize = 8;
const DEFAULT_INCLUDE_TRACK_CAPACITY: usize = 32;

/// Source of an included file
pub enum IncludeSource {
    /// File on disk
    File(PathBuf),
    /// Builtin (embedded) header content (content only, name is for display)
    Builtin(&'static str), // content
}

// ============================================================================
// Macro Definition
// ============================================================================

/// A macro parameter
#[derive(Debug, Clone)]
pub struct MacroParam {
    pub name: String,
    pub index: usize,
}

/// A macro definition (object-like or function-like)
#[derive(Debug, Clone)]
pub struct Macro {
    /// Macro name
    pub name: String,
    /// Replacement tokens (stored as token copies)
    pub body: Vec<MacroToken>,
    /// Is this a function-like macro?
    pub is_function: bool,
    /// Parameters for function-like macros
    pub params: Vec<MacroParam>,
    /// Is this a variadic macro (...)? (reserved for future use)
    pub _is_variadic: bool,
    /// Built-in expand function (for __LINE__, __FILE__, etc.)
    pub builtin: Option<BuiltinMacro>,
}

/// A token stored in a macro body
#[derive(Debug, Clone)]
pub struct MacroToken {
    pub typ: TokenType,
    pub value: MacroTokenValue,
    pub whitespace: bool,
}

/// Value of a macro token
#[derive(Debug, Clone)]
pub enum MacroTokenValue {
    None,
    Number(String),
    Ident(String),
    String(String),
    Char(String),
    Special(u32),
    /// Parameter reference (by index)
    Param(usize),
    /// Stringified parameter (#param)
    Stringify(usize),
    /// Token paste marker (##)
    Paste,
    /// __VA_ARGS__ (index of variadic params start)
    VaArgs,
}

/// Built-in macro types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinMacro {
    Line,
    File,
    Date,
    Time,
    Counter,
    IncludeLevel,
    BaseFile,
    HasAttribute,
    HasBuiltin,
    HasFeature,
    HasExtension,
    HasInclude,
    HasIncludeNext,
}

impl Macro {
    /// Create a predefined macro (value is treated as a number/literal)
    pub fn predefined(name: &str, value: Option<&str>) -> Self {
        let body = match value {
            Some(v) => vec![MacroToken {
                typ: TokenType::Number,
                value: MacroTokenValue::Number(v.to_string()),
                whitespace: false,
            }],
            None => vec![],
        };
        Self {
            name: name.to_string(),
            body,
            is_function: false,
            params: vec![],
            _is_variadic: false,
            builtin: None,
        }
    }

    /// Create a predefined type macro (value is tokenized as identifiers)
    /// This is used for macros like __PTRDIFF_TYPE__ that expand to type names
    pub fn predefined_type(name: &str, value: &str) -> Self {
        // Tokenize the value into separate identifier tokens
        let words: Vec<&str> = value.split_whitespace().collect();
        let body: Vec<MacroToken> = words
            .into_iter()
            .enumerate()
            .map(|(i, word)| MacroToken {
                typ: TokenType::Ident,
                value: MacroTokenValue::Ident(word.to_string()),
                whitespace: i > 0, // Add whitespace before all but the first token
            })
            .collect();
        Self {
            name: name.to_string(),
            body,
            is_function: false,
            params: vec![],
            _is_variadic: false,
            builtin: None,
        }
    }

    /// Create a predefined macro from a command-line -D value.
    /// The value is tokenized properly so -DFOO=123 becomes a number token,
    /// -DFOO=bar becomes an identifier token, etc.
    pub fn from_cmdline_define(name: &str, value: &str) -> Self {
        // Tokenize the value string
        let mut idents = IdentTable::new();
        let mut tokenizer = Tokenizer::new(value.as_bytes(), 0, &mut idents);
        let tokens = tokenizer.tokenize();

        // Convert tokens to macro tokens, skipping stream markers
        let body: Vec<MacroToken> = tokens
            .iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .enumerate()
            .map(|(i, token)| {
                let value = match &token.value {
                    TokenValue::Number(n) => MacroTokenValue::Number(n.clone()),
                    TokenValue::Ident(id) => {
                        let ident_name = idents.get_opt(*id).unwrap_or("").to_string();
                        MacroTokenValue::Ident(ident_name)
                    }
                    TokenValue::String(s) => MacroTokenValue::String(s.clone()),
                    TokenValue::Char(c) => MacroTokenValue::Char(c.clone()),
                    TokenValue::Special(code) => MacroTokenValue::Special(*code),
                    TokenValue::WideString(s) => MacroTokenValue::String(s.clone()),
                    TokenValue::WideChar(c) => MacroTokenValue::Char(c.clone()),
                    TokenValue::None => MacroTokenValue::None,
                };
                MacroToken {
                    typ: token.typ,
                    value,
                    whitespace: i > 0 && token.pos.whitespace,
                }
            })
            .collect();

        Self {
            name: name.to_string(),
            body,
            is_function: false,
            params: vec![],
            _is_variadic: false,
            builtin: None,
        }
    }

    /// Create a keyword alias macro (value is treated as an identifier/keyword)
    pub fn keyword_alias(name: &str, value: &str) -> Self {
        let body = if value.is_empty() {
            vec![]
        } else {
            vec![MacroToken {
                typ: TokenType::Ident,
                value: MacroTokenValue::Ident(value.to_string()),
                whitespace: false,
            }]
        };
        Self {
            name: name.to_string(),
            body,
            is_function: false,
            params: vec![],
            _is_variadic: false,
            builtin: None,
        }
    }

    /// Create a builtin macro
    pub fn builtin(name: &str, builtin: BuiltinMacro, is_function: bool) -> Self {
        Self {
            name: name.to_string(),
            body: vec![],
            is_function,
            params: if is_function {
                vec![MacroParam {
                    name: "x".to_string(),
                    index: 0,
                }]
            } else {
                vec![]
            },
            _is_variadic: false,
            builtin: Some(builtin),
        }
    }
}

// ============================================================================
// Conditional Compilation State
// ============================================================================

/// State of a conditional (#if/#ifdef block)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CondState {
    /// Currently in a true branch, processing tokens
    Active,
    /// Currently in a false branch, skipping tokens
    Skipping,
    /// Already found a true branch, skip remaining branches
    Done,
}

/// A conditional compilation block
#[derive(Debug, Clone)]
struct Conditional {
    state: CondState,
    /// Has this conditional had a true branch?
    had_true: bool,
    /// Position of the #if/#ifdef/#ifndef directive
    pos: Position,
}

// ============================================================================
// Preprocessor
// ============================================================================

/// C Preprocessor
pub struct Preprocessor<'a> {
    /// Target configuration
    target: &'a Target,

    /// Macro definitions
    macros: HashMap<String, Macro>,

    /// Conditional compilation stack
    cond_stack: Vec<Conditional>,

    /// Include paths for angle-bracket includes
    system_include_paths: Vec<String>,

    /// Include paths for quote includes (searched first)
    quote_include_paths: Vec<String>,

    /// Current file name (for __FILE__)
    current_file: String,

    /// Base file name (for __BASE_FILE__ - the main input file)
    base_file: String,

    /// Current file directory (for relative includes)
    current_dir: String,

    /// Counter for __COUNTER__
    counter: u32,

    /// Include depth (for cycle detection)
    include_depth: u32,

    /// Maximum include depth
    max_include_depth: u32,

    /// Set of files currently being included (cycle detection)
    include_stack: HashSet<PathBuf>,

    /// Files that have been included with #pragma once or include guards
    once_files: HashSet<PathBuf>,

    /// Macros currently being expanded (for recursion prevention)
    expanding: HashSet<String>,

    /// Compilation date string for __DATE__ (format: "Mmm dd yyyy")
    compile_date: String,

    /// Compilation time string for __TIME__ (format: "hh:mm:ss")
    compile_time: String,

    /// Preprocess call depth (0 = top level, >0 = recursive for macros/includes)
    preprocess_depth: u32,

    /// Whether to use builtin headers (disabled by -nobuiltininc or -nostdinc)
    use_builtin_headers: bool,

    /// Whether to use system include paths (disabled by -nostdinc)
    use_system_headers: bool,

    /// Index of current file's system include path (for #include_next)
    /// None if current file is not from a system include path
    current_include_path_index: Option<usize>,
}

impl<'a> Preprocessor<'a> {
    /// Format the current time as C99 __DATE__ and __TIME__ strings
    /// Returns (date_string, time_string) where:
    /// - date_string is "Mmm dd yyyy" (e.g., "Dec  4 2025")
    /// - time_string is "hh:mm:ss" (e.g., "14:30:00")
    fn format_compile_time() -> (String, String) {
        const MONTHS: [&str; 12] = [
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
        ];

        // Get seconds since Unix epoch
        let duration = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default();
        let secs = duration.as_secs() as i64;

        // Convert to date/time components
        // Days since epoch
        let days = secs / 86400;
        let time_of_day = secs % 86400;

        let hours = time_of_day / 3600;
        let minutes = (time_of_day % 3600) / 60;
        let seconds = time_of_day % 60;

        // Calculate year, month, day from days since epoch (1970-01-01)
        // Using a simplified algorithm
        let mut remaining_days = days;
        let mut year = 1970i32;

        loop {
            let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                366
            } else {
                365
            };
            if remaining_days < days_in_year {
                break;
            }
            remaining_days -= days_in_year;
            year += 1;
        }

        let is_leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
        let days_in_months: [i64; 12] = if is_leap {
            [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        } else {
            [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        };

        let mut month = 0usize;
        for (i, &days_in_month) in days_in_months.iter().enumerate() {
            if remaining_days < days_in_month {
                month = i;
                break;
            }
            remaining_days -= days_in_month;
        }
        let day = remaining_days + 1; // 1-based

        // Format date: "Mmm dd yyyy" (day is space-padded to 2 chars)
        let date_str = format!("{} {:2} {}", MONTHS[month], day, year);
        // Format time: "hh:mm:ss"
        let time_str = format!("{:02}:{:02}:{:02}", hours, minutes, seconds);

        (date_str, time_str)
    }

    /// Create a new preprocessor
    pub fn new(target: &'a Target, filename: &str) -> Self {
        let current_dir = Path::new(filename)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| ".".to_string());

        let (compile_date, compile_time) = Self::format_compile_time();

        let mut pp = Self {
            target,
            macros: HashMap::with_capacity(DEFAULT_MACRO_CAPACITY),
            cond_stack: Vec::with_capacity(DEFAULT_COND_STACK_CAPACITY),
            system_include_paths: Vec::with_capacity(DEFAULT_INCLUDE_PATH_CAPACITY),
            quote_include_paths: Vec::with_capacity(DEFAULT_INCLUDE_PATH_CAPACITY),
            current_file: filename.to_string(),
            base_file: filename.to_string(),
            current_dir,
            counter: 0,
            include_depth: 0,
            max_include_depth: 200,
            include_stack: HashSet::with_capacity(DEFAULT_INCLUDE_TRACK_CAPACITY),
            once_files: HashSet::with_capacity(DEFAULT_INCLUDE_TRACK_CAPACITY),
            expanding: HashSet::with_capacity(DEFAULT_COND_STACK_CAPACITY),
            compile_date,
            compile_time,
            preprocess_depth: 0,
            use_builtin_headers: true,
            use_system_headers: true,
            current_include_path_index: None,
        };

        // Initialize predefined macros
        pp.init_predefined_macros();

        // Initialize include paths from OS
        for path in os::get_include_paths(target) {
            pp.system_include_paths.push(path.to_string());
        }

        pp
    }

    /// Initialize predefined macros
    fn init_predefined_macros(&mut self) {
        // Standard C macros
        self.define_macro(Macro::predefined("__STDC__", Some("1")));
        self.define_macro(Macro::predefined("__STDC_VERSION__", Some("201112L"))); // C11
        self.define_macro(Macro::predefined("__STDC_HOSTED__", Some("1")));

        // GCC compatibility macros (required by system headers)
        self.define_macro(Macro::predefined("__GNUC__", Some("4")));
        self.define_macro(Macro::predefined("__GNUC_MINOR__", Some("2")));
        self.define_macro(Macro::predefined("__GNUC_PATCHLEVEL__", Some("1")));
        self.define_macro(Macro::predefined(
            "__VERSION__",
            Some(concat!(
                "\"pcc ",
                env!("CARGO_PKG_VERSION"),
                " (gcc compatible 4.2.1)\""
            )),
        ));
        self.define_macro(Macro::predefined("__GNUC_STDC_INLINE__", Some("1")));

        // GCC type keyword compatibility
        self.define_macro(Macro::keyword_alias("__signed", "signed"));
        self.define_macro(Macro::keyword_alias("__signed__", "signed"));
        self.define_macro(Macro::keyword_alias("__inline", "inline"));
        self.define_macro(Macro::keyword_alias("__inline__", "inline"));
        self.define_macro(Macro::keyword_alias("__volatile", "volatile"));
        self.define_macro(Macro::keyword_alias("__volatile__", "volatile"));
        self.define_macro(Macro::keyword_alias("__extension__", "")); // expands to nothing
        self.define_macro(Macro::keyword_alias("__restrict", "restrict"));
        self.define_macro(Macro::keyword_alias("__restrict__", "restrict"));

        // C11 atomic memory order constants (GCC-compatible for <stdatomic.h>)
        self.define_macro(Macro::predefined("__ATOMIC_RELAXED", Some("0")));
        self.define_macro(Macro::predefined("__ATOMIC_CONSUME", Some("1")));
        self.define_macro(Macro::predefined("__ATOMIC_ACQUIRE", Some("2")));
        self.define_macro(Macro::predefined("__ATOMIC_RELEASE", Some("3")));
        self.define_macro(Macro::predefined("__ATOMIC_ACQ_REL", Some("4")));
        self.define_macro(Macro::predefined("__ATOMIC_SEQ_CST", Some("5")));

        // Architecture macros
        for (name, value) in arch::get_arch_macros(self.target) {
            if let Some(v) = value {
                self.define_macro(Macro::predefined(name, Some(v)));
            } else {
                self.define_macro(Macro::predefined(name, None));
            }
        }

        // Limit macros
        for (name, value) in arch::get_limit_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Type definition macros (for <stdint.h> and <stddef.h>)
        // These expand to type names, so they need to be tokenized properly
        for (name, value) in arch::get_type_macros(self.target) {
            self.define_macro(Macro::predefined_type(name, value));
        }

        // Fixed-width integer limit macros (for <stdint.h>)
        for (name, value) in arch::get_stdint_limit_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Integer constant suffix macros
        for (name, value) in arch::get_suffix_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Format specifier macros (for <inttypes.h>)
        for (name, value) in arch::get_format_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Additional sizeof macros
        for (name, value) in arch::get_additional_sizeof_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Miscellaneous macros
        for (name, value) in arch::get_misc_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // Floating-point limit macros
        for (name, value) in arch::get_float_limit_macros(self.target) {
            self.define_macro(Macro::predefined(name, Some(value)));
        }

        // OS macros
        for (name, value) in os::get_os_macros(self.target) {
            if let Some(v) = value {
                self.define_macro(Macro::predefined(name, Some(v)));
            } else {
                self.define_macro(Macro::predefined(name, None));
            }
        }

        // Builtin macros
        self.define_macro(Macro::builtin("__LINE__", BuiltinMacro::Line, false));
        self.define_macro(Macro::builtin("__FILE__", BuiltinMacro::File, false));
        self.define_macro(Macro::builtin("__DATE__", BuiltinMacro::Date, false));
        self.define_macro(Macro::builtin("__TIME__", BuiltinMacro::Time, false));
        self.define_macro(Macro::builtin("__COUNTER__", BuiltinMacro::Counter, false));
        self.define_macro(Macro::builtin(
            "__INCLUDE_LEVEL__",
            BuiltinMacro::IncludeLevel,
            false,
        ));
        self.define_macro(Macro::builtin(
            "__BASE_FILE__",
            BuiltinMacro::BaseFile,
            false,
        ));

        // Function-like builtins
        self.define_macro(Macro::builtin(
            "__has_attribute",
            BuiltinMacro::HasAttribute,
            true,
        ));
        self.define_macro(Macro::builtin(
            "__has_builtin",
            BuiltinMacro::HasBuiltin,
            true,
        ));
        self.define_macro(Macro::builtin(
            "__has_feature",
            BuiltinMacro::HasFeature,
            true,
        ));
        self.define_macro(Macro::builtin(
            "__has_extension",
            BuiltinMacro::HasExtension,
            true,
        ));
        self.define_macro(Macro::builtin(
            "__has_include",
            BuiltinMacro::HasInclude,
            true,
        ));
        self.define_macro(Macro::builtin(
            "__has_include_next",
            BuiltinMacro::HasIncludeNext,
            true,
        ));
    }

    /// Define a macro
    pub fn define_macro(&mut self, mac: Macro) {
        self.macros.insert(mac.name.clone(), mac);
    }

    /// Undefine a macro
    pub fn undef_macro(&mut self, name: &str) {
        self.macros.remove(name);
    }

    /// Check if a macro is defined
    pub fn is_defined(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Get a macro definition
    pub fn get_macro(&self, name: &str) -> Option<&Macro> {
        self.macros.get(name)
    }

    /// Check if we're currently skipping tokens
    fn is_skipping(&self) -> bool {
        self.cond_stack
            .last()
            .map(|c| c.state != CondState::Active)
            .unwrap_or(false)
    }

    /// Process tokens through the preprocessor
    pub fn preprocess(&mut self, tokens: Vec<Token>, idents: &mut IdentTable) -> Vec<Token> {
        self.preprocess_depth += 1;
        let mut output = Vec::new();
        let mut iter = tokens.into_iter().peekable();

        while let Some(token) = iter.next() {
            match token.typ {
                TokenType::StreamBegin | TokenType::StreamEnd => {
                    // Pass through stream markers
                    output.push(token);
                }

                TokenType::Special => {
                    if let TokenValue::Special(code) = &token.value {
                        // Check for # at start of line (preprocessor directive)
                        if *code == b'#' as u32 && token.pos.newline {
                            self.handle_directive(&mut iter, &token, &mut output, idents);
                            continue;
                        }
                    }
                    if !self.is_skipping() {
                        output.push(token);
                    }
                }

                TokenType::Ident => {
                    if self.is_skipping() {
                        continue;
                    }
                    // Check for macro expansion
                    if let TokenValue::Ident(id) = &token.value {
                        if let Some(name) = idents.get_opt(*id) {
                            let name = name.to_string();

                            // Handle _Pragma operator (C99)
                            // _Pragma("string") is equivalent to #pragma string
                            // We silently consume it since we ignore #pragma anyway
                            if name == "_Pragma" {
                                self.handle_pragma_operator(&mut iter);
                                continue;
                            }

                            if let Some(expanded) =
                                self.try_expand_macro(&name, &token.pos, &mut iter, idents)
                            {
                                output.extend(expanded);

                                // Check if the last expanded token is a function-like macro
                                // followed by '(' in the remaining input stream.
                                // This handles cases like: CALL(ADD)(10, 32) where token paste
                                // creates a function-like macro name and args come from outside.
                                while let Some(last) = output.last() {
                                    if last.typ != TokenType::Ident {
                                        break;
                                    }
                                    let TokenValue::Ident(id) = &last.value else {
                                        break;
                                    };
                                    let Some(macro_name) = idents.get_opt(*id) else {
                                        break;
                                    };
                                    let macro_name = macro_name.to_string();

                                    // Check if it's a function-like macro
                                    let Some(mac) = self.macros.get(&macro_name) else {
                                        break;
                                    };
                                    if !mac.is_function {
                                        break;
                                    }

                                    // Check if next token is '('
                                    let Some(next) = iter.peek() else { break };
                                    let TokenValue::Special(code) = &next.value else {
                                        break;
                                    };
                                    if *code != b'(' as u32 {
                                        break;
                                    }

                                    // Check for recursion
                                    if self.expanding.contains(&macro_name) {
                                        break;
                                    }

                                    // Pop the identifier and expand as function-like macro
                                    let last_token = output.pop().unwrap();
                                    iter.next(); // consume '('
                                    let args = self.collect_macro_args(
                                        &mut iter,
                                        idents,
                                        &last_token.pos,
                                        &macro_name,
                                    );
                                    let mac = mac.clone();
                                    if let Some(more_expanded) = self.expand_function_macro(
                                        &mac,
                                        &args,
                                        &last_token.pos,
                                        idents,
                                    ) {
                                        output.extend(more_expanded);
                                        // Loop to handle chained expansions
                                    } else {
                                        // Put back the identifier if expansion failed
                                        output.push(last_token);
                                        break;
                                    }
                                }

                                continue;
                            }
                        }
                    }
                    output.push(token);
                }

                _ => {
                    if !self.is_skipping() {
                        output.push(token);
                    }
                }
            }
        }

        // Check for unterminated conditionals (only at top level)
        // - preprocess_depth == 1: we're at the outermost preprocess call (not recursive)
        // - include_depth == 0: we're not inside an included file
        // Recursive calls happen during macro expansion and include file processing
        if self.preprocess_depth == 1 && self.include_depth == 0 && !self.cond_stack.is_empty() {
            // Report location of first unterminated #if directive
            let first_pos = self.cond_stack.first().map(|c| c.pos).unwrap_or_default();
            let msg = format!("{} unterminated #if directive(s)", self.cond_stack.len());
            diag::warning(first_pos, &msg);
        }

        self.preprocess_depth -= 1;
        output
    }

    /// Handle a preprocessor directive
    fn handle_directive<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        hash_token: &Token,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
    ) where
        I: Iterator<Item = Token>,
    {
        // Get the directive name
        let directive_token = match iter.next() {
            Some(t) => t,
            None => return, // Empty directive, ignore
        };

        // Get directive name
        let directive_name = match &directive_token.typ {
            TokenType::Ident => {
                if let TokenValue::Ident(id) = &directive_token.value {
                    idents.get_opt(*id).map(|s| s.to_string())
                } else {
                    None
                }
            }
            _ => None,
        };

        let directive = match directive_name {
            Some(name) => name,
            None => {
                // Consume rest of line
                self.skip_to_eol(iter);
                return;
            }
        };

        match directive.as_str() {
            "define" => self.handle_define(iter, idents),
            "undef" => self.handle_undef(iter, idents),
            "ifdef" => self.handle_ifdef(iter, idents, hash_token.pos),
            "ifndef" => self.handle_ifndef(iter, idents, hash_token.pos),
            "if" => self.handle_if(iter, idents, hash_token.pos),
            "elif" => self.handle_elif(iter, idents),
            "else" => self.handle_else(iter),
            "endif" => self.handle_endif(iter),
            "include" => self.handle_include(iter, output, idents, hash_token, false),
            "include_next" => self.handle_include(iter, output, idents, hash_token, true),
            "error" => self.handle_error(iter, &hash_token.pos, idents),
            "warning" => self.handle_warning(iter, &hash_token.pos, idents),
            "pragma" => self.handle_pragma(iter, idents),
            "line" => self.handle_line(iter),
            _ => {
                // Unknown directive
                if !self.is_skipping() {
                    diag::warning(
                        hash_token.pos,
                        &format!("unknown preprocessor directive #{}", directive),
                    );
                }
                self.skip_to_eol(iter);
            }
        }
    }

    /// Skip tokens until end of line
    fn skip_to_eol<I>(&self, iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = Token>,
    {
        while let Some(token) = iter.peek() {
            if token.pos.newline {
                break;
            }
            iter.next();
        }
    }

    /// Collect tokens until end of line
    fn collect_to_eol<I>(&self, iter: &mut std::iter::Peekable<I>) -> Vec<Token>
    where
        I: Iterator<Item = Token>,
    {
        let mut tokens = Vec::new();
        while let Some(token) = iter.peek() {
            if token.pos.newline {
                break;
            }
            tokens.push(iter.next().unwrap());
        }
        tokens
    }

    /// Expand macros in #if/#elif condition tokens.
    /// This follows the C standard: macros are expanded, except for arguments to `defined`.
    /// Undefined identifiers are replaced with 0.
    fn expand_if_tokens(&mut self, tokens: &[Token], idents: &mut IdentTable) -> Vec<Token> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            let token = &tokens[i];

            // Check for `defined` operator
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    if name == "defined" {
                        // Handle defined(X) or defined X
                        let pos = token.pos;
                        i += 1;

                        // Skip optional whitespace and check for paren
                        let has_paren = if i < tokens.len() {
                            if let TokenValue::Special(code) = &tokens[i].value {
                                *code == b'(' as u32
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if has_paren {
                            i += 1; // skip '('
                        }

                        // Get the identifier to check
                        let is_defined = if i < tokens.len() {
                            if let TokenValue::Ident(check_id) = &tokens[i].value {
                                if let Some(check_name) = idents.get_opt(*check_id) {
                                    i += 1;
                                    self.is_defined(check_name)
                                } else {
                                    i += 1;
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if has_paren {
                            // Skip closing ')'
                            if i < tokens.len() {
                                if let TokenValue::Special(code) = &tokens[i].value {
                                    if *code == b')' as u32 {
                                        i += 1;
                                    }
                                }
                            }
                        }

                        // Replace with 0 or 1
                        result.push(Token {
                            typ: TokenType::Number,
                            value: TokenValue::Number(if is_defined {
                                "1".to_string()
                            } else {
                                "0".to_string()
                            }),
                            pos,
                        });
                        continue;
                    }
                }
            }

            // Not `defined` - collect this token for expansion
            result.push(token.clone());
            i += 1;
        }

        // Now expand macros in the result (except `defined` which we already handled)
        // We need to temporarily disable skipping because we're evaluating an expression
        // that will determine whether to skip. Push a dummy "Active" conditional.
        self.cond_stack.push(Conditional {
            state: CondState::Active,
            had_true: true,
            pos: Position::default(),
        });
        let expanded = self.preprocess(result, idents);
        self.cond_stack.pop();

        // Replace any remaining undefined identifiers with 0
        let mut final_result = Vec::new();
        for token in expanded {
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    // Check if it's a defined macro
                    if self.get_macro(name).is_some() {
                        // This shouldn't happen after expansion, but keep it
                        final_result.push(token);
                    } else {
                        // Undefined identifier -> 0
                        final_result.push(Token {
                            typ: TokenType::Number,
                            value: TokenValue::Number("0".to_string()),
                            pos: token.pos,
                        });
                    }
                } else {
                    // Unknown identifier -> 0
                    final_result.push(Token {
                        typ: TokenType::Number,
                        value: TokenValue::Number("0".to_string()),
                        pos: token.pos,
                    });
                }
            } else {
                final_result.push(token);
            }
        }

        final_result
    }

    /// Handle #define
    fn handle_define<I>(&mut self, iter: &mut std::iter::Peekable<I>, idents: &IdentTable)
    where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Get macro name
        let name_token = match iter.next() {
            Some(t) => t,
            None => return,
        };

        let macro_name = match &name_token.typ {
            TokenType::Ident => {
                if let TokenValue::Ident(id) = &name_token.value {
                    idents.get_opt(*id).map(|s| s.to_string())
                } else {
                    None
                }
            }
            _ => None,
        };

        let name = match macro_name {
            Some(n) => n,
            None => {
                self.skip_to_eol(iter);
                return;
            }
        };

        // Check if function-like macro (immediate '(' without whitespace)
        let mut params = Vec::new();
        let mut is_function = false;
        let mut _is_variadic = false;

        if let Some(next) = iter.peek() {
            if !next.pos.whitespace {
                if let TokenValue::Special(code) = &next.value {
                    if *code == b'(' as u32 {
                        is_function = true;
                        iter.next(); // consume '('

                        // Parse parameters
                        let mut param_index = 0;
                        while let Some(param_tok) = iter.next() {
                            if param_tok.pos.newline {
                                break;
                            }

                            match &param_tok.value {
                                TokenValue::Special(c) if *c == b')' as u32 => break,
                                TokenValue::Special(c) if *c == b',' as u32 => continue,
                                TokenValue::Special(c) if *c == SpecialToken::Ellipsis as u32 => {
                                    _is_variadic = true;
                                    // Consume closing paren
                                    for t in iter.by_ref() {
                                        if t.pos.newline {
                                            break;
                                        }
                                        if let TokenValue::Special(c) = &t.value {
                                            if *c == b')' as u32 {
                                                break;
                                            }
                                        }
                                    }
                                    break;
                                }
                                TokenValue::Ident(id) => {
                                    if let Some(param_name) = idents.get_opt(*id) {
                                        params.push(MacroParam {
                                            name: param_name.to_string(),
                                            index: param_index,
                                        });
                                        param_index += 1;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }

        // Collect body tokens
        let body_tokens = self.collect_to_eol(iter);
        let body = self.tokens_to_macro_body(&body_tokens, &params, idents);

        let mac = Macro {
            name: name.clone(),
            body,
            is_function,
            params,
            _is_variadic,
            builtin: None,
        };

        self.define_macro(mac);
    }

    /// Convert tokens to macro body
    fn tokens_to_macro_body(
        &self,
        tokens: &[Token],
        params: &[MacroParam],
        idents: &IdentTable,
    ) -> Vec<MacroToken> {
        let mut body = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            let token = &tokens[i];

            // Check for # (stringify)
            if let TokenValue::Special(code) = &token.value {
                if *code == b'#' as u32 {
                    // Check if followed by ##
                    if i + 1 < tokens.len() {
                        if let TokenValue::Special(next_code) = &tokens[i + 1].value {
                            if *next_code == b'#' as u32 {
                                // ## token paste
                                body.push(MacroToken {
                                    typ: TokenType::Special,
                                    value: MacroTokenValue::Paste,
                                    whitespace: token.pos.whitespace,
                                });
                                i += 2;
                                continue;
                            }
                        }
                    }

                    // # stringify - look for following parameter
                    if i + 1 < tokens.len() {
                        if let TokenValue::Ident(id) = &tokens[i + 1].value {
                            if let Some(name) = idents.get_opt(*id) {
                                // Check if it's a parameter
                                let mut found_param = false;
                                for param in params {
                                    if param.name == name {
                                        body.push(MacroToken {
                                            typ: TokenType::Special,
                                            value: MacroTokenValue::Stringify(param.index),
                                            whitespace: token.pos.whitespace,
                                        });
                                        found_param = true;
                                        break;
                                    }
                                }
                                if found_param {
                                    i += 2;
                                    continue;
                                }
                                // Check for __VA_ARGS__
                                if name == "__VA_ARGS__" {
                                    body.push(MacroToken {
                                        typ: TokenType::Special,
                                        value: MacroTokenValue::Stringify(params.len()),
                                        whitespace: token.pos.whitespace,
                                    });
                                    i += 2;
                                    continue;
                                }
                            }
                        }
                    }
                }

                // Check for ## (token paste) - also handle HashHash special token
                if *code == SpecialToken::HashHash as u32 {
                    body.push(MacroToken {
                        typ: TokenType::Special,
                        value: MacroTokenValue::Paste,
                        whitespace: token.pos.whitespace,
                    });
                    i += 1;
                    continue;
                }
            }

            // Check for parameter reference or __VA_ARGS__
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    // Check if it's __VA_ARGS__
                    if name == "__VA_ARGS__" {
                        body.push(MacroToken {
                            typ: TokenType::Ident,
                            value: MacroTokenValue::VaArgs,
                            whitespace: token.pos.whitespace,
                        });
                        i += 1;
                        continue;
                    }

                    // Check if it's a parameter
                    let mut found_param = false;
                    for param in params {
                        if param.name == name {
                            body.push(MacroToken {
                                typ: TokenType::Ident,
                                value: MacroTokenValue::Param(param.index),
                                whitespace: token.pos.whitespace,
                            });
                            found_param = true;
                            break;
                        }
                    }
                    if found_param {
                        i += 1;
                        continue;
                    }
                }
            }

            // Regular token
            let macro_token = self.token_to_macro_token(token, idents);
            body.push(macro_token);
            i += 1;
        }

        body
    }

    /// Convert a regular token to a macro token
    fn token_to_macro_token(&self, token: &Token, idents: &IdentTable) -> MacroToken {
        let value = match &token.value {
            TokenValue::Number(n) => MacroTokenValue::Number(n.clone()),
            TokenValue::Ident(id) => {
                let name = idents.get_opt(*id).unwrap_or("").to_string();
                MacroTokenValue::Ident(name)
            }
            TokenValue::String(s) => MacroTokenValue::String(s.clone()),
            TokenValue::Char(c) => MacroTokenValue::Char(c.clone()),
            TokenValue::Special(code) => MacroTokenValue::Special(*code),
            TokenValue::WideString(s) => MacroTokenValue::String(s.clone()),
            TokenValue::WideChar(c) => MacroTokenValue::Char(c.clone()),
            TokenValue::None => MacroTokenValue::None,
        };

        MacroToken {
            typ: token.typ,
            value,
            whitespace: token.pos.whitespace,
        }
    }

    /// Handle #undef
    fn handle_undef<I>(&mut self, iter: &mut std::iter::Peekable<I>, idents: &IdentTable)
    where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        if let Some(token) = iter.next() {
            if let TokenType::Ident = &token.typ {
                if let TokenValue::Ident(id) = &token.value {
                    if let Some(name) = idents.get_opt(*id) {
                        self.undef_macro(name);
                    }
                }
            }
        }

        self.skip_to_eol(iter);
    }

    /// Handle #ifdef
    fn handle_ifdef<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        idents: &IdentTable,
        pos: Position,
    ) where
        I: Iterator<Item = Token>,
    {
        let defined = if let Some(token) = iter.next() {
            if let TokenType::Ident = &token.typ {
                if let TokenValue::Ident(id) = &token.value {
                    if let Some(name) = idents.get_opt(*id) {
                        self.is_defined(name)
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        self.skip_to_eol(iter);
        self.push_conditional(defined, pos);
    }

    /// Handle #ifndef
    fn handle_ifndef<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        idents: &IdentTable,
        pos: Position,
    ) where
        I: Iterator<Item = Token>,
    {
        let defined = if let Some(token) = iter.next() {
            if let TokenType::Ident = &token.typ {
                if let TokenValue::Ident(id) = &token.value {
                    if let Some(name) = idents.get_opt(*id) {
                        self.is_defined(name)
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        self.skip_to_eol(iter);
        self.push_conditional(!defined, pos);
    }

    /// Handle #if
    fn handle_if<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        idents: &mut IdentTable,
        pos: Position,
    ) where
        I: Iterator<Item = Token>,
    {
        let tokens = self.collect_to_eol(iter);
        let value = if self.is_skipping() {
            false
        } else {
            // Expand macros before evaluation (per C standard)
            let expanded = self.expand_if_tokens(&tokens, idents);
            self.evaluate_expression(&expanded, idents)
        };

        self.push_conditional(value, pos);
    }

    /// Handle #elif
    fn handle_elif<I>(&mut self, iter: &mut std::iter::Peekable<I>, idents: &mut IdentTable)
    where
        I: Iterator<Item = Token>,
    {
        let tokens = self.collect_to_eol(iter);

        // Check if we should evaluate this branch
        let should_eval = if let Some(cond) = self.cond_stack.last() {
            cond.state == CondState::Skipping && !cond.had_true
        } else {
            false
        };

        let expr_value = if should_eval {
            // Expand macros before evaluation (per C standard)
            let expanded = self.expand_if_tokens(&tokens, idents);
            self.evaluate_expression(&expanded, idents)
        } else {
            false
        };

        if let Some(cond) = self.cond_stack.last_mut() {
            match cond.state {
                CondState::Active => {
                    // We were in a true branch, now skip
                    cond.state = CondState::Done;
                    cond.had_true = true;
                }
                CondState::Skipping => {
                    if !cond.had_true && expr_value {
                        // Try this branch
                        cond.state = CondState::Active;
                        cond.had_true = true;
                    }
                }
                CondState::Done => {
                    // Already found true branch, skip
                }
            }
        }
    }

    /// Handle #else
    fn handle_else<I>(&mut self, iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = Token>,
    {
        self.skip_to_eol(iter);

        if let Some(cond) = self.cond_stack.last_mut() {
            match cond.state {
                CondState::Active => {
                    cond.state = CondState::Done;
                    cond.had_true = true;
                }
                CondState::Skipping => {
                    if !cond.had_true {
                        cond.state = CondState::Active;
                        cond.had_true = true;
                    } else {
                        cond.state = CondState::Done;
                    }
                }
                CondState::Done => {}
            }
        }
    }

    /// Handle #endif
    fn handle_endif<I>(&mut self, iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = Token>,
    {
        self.skip_to_eol(iter);
        self.cond_stack.pop();
    }

    /// Push a new conditional
    fn push_conditional(&mut self, condition: bool, pos: Position) {
        // If we're already skipping, new conditional starts in skip mode
        let parent_skipping = self.is_skipping();

        let state = if parent_skipping {
            CondState::Skipping
        } else if condition {
            CondState::Active
        } else {
            CondState::Skipping
        };

        self.cond_stack.push(Conditional {
            state,
            // When parent is skipping, mark had_true so #else/#elif won't activate
            had_true: parent_skipping || condition,
            pos,
        });
    }

    /// Evaluate a preprocessor expression
    fn evaluate_expression(&self, tokens: &[Token], idents: &IdentTable) -> bool {
        let mut evaluator = ExprEvaluator::new(self, idents);
        evaluator.evaluate(tokens) != 0
    }

    /// Handle #include
    fn handle_include<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
        hash_token: &Token,
        is_include_next: bool,
    ) where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Collect the include path tokens
        let path_tokens = self.collect_to_eol(iter);
        if path_tokens.is_empty() {
            diag::error(hash_token.pos, "expected filename after #include");
            return;
        }

        // Check if we need macro expansion (C99 6.10.2)
        // If the first token is not < or ", expand macros first
        let needs_expansion = match &path_tokens[0].value {
            TokenValue::Special(code) => *code != b'<' as u32,
            TokenValue::String(_) => false, // Already a string literal
            _ => true,                      // Identifier or other - needs expansion
        };

        let expanded_tokens = if needs_expansion {
            // Expand macros in the include path
            // Push temporary conditional to enable preprocessing
            self.cond_stack.push(Conditional {
                state: CondState::Active,
                had_true: true,
                pos: Position::default(),
            });
            let expanded = self.preprocess(path_tokens, idents);
            self.cond_stack.pop();
            expanded
        } else {
            path_tokens
        };

        // Determine if system include (<...>) or quoted ("...")
        let (filename, is_system) = self.parse_include_path(&expanded_tokens, idents);

        if filename.is_empty() {
            diag::error(hash_token.pos, "empty filename in #include");
            return;
        }

        // Find and include the file
        if let Some((source, path_index)) =
            self.find_include_file(&filename, is_system, is_include_next)
        {
            match source {
                IncludeSource::File(path) => {
                    self.include_file(&path, output, idents, hash_token, path_index);
                }
                IncludeSource::Builtin(content) => {
                    self.include_builtin(&filename, content, output, idents, hash_token);
                }
            }
        } else {
            diag::error(hash_token.pos, &format!("'{}': file not found", filename));
        }
    }

    /// Parse include path from tokens
    fn parse_include_path(&self, tokens: &[Token], idents: &IdentTable) -> (String, bool) {
        if tokens.is_empty() {
            return (String::new(), false);
        }

        // Check for <filename>
        if let TokenValue::Special(code) = &tokens[0].value {
            if *code == b'<' as u32 {
                // System include - collect until >
                let mut filename = String::new();
                for token in &tokens[1..] {
                    if let TokenValue::Special(c) = &token.value {
                        if *c == b'>' as u32 {
                            break;
                        }
                        filename.push(*c as u8 as char);
                    } else {
                        filename.push_str(&self.token_to_string(token, idents));
                    }
                }
                return (filename, true);
            }
        }

        // Check for "filename"
        if let TokenValue::String(s) = &tokens[0].value {
            return (s.clone(), false);
        }

        // Fallback: try to reconstruct from tokens
        let mut filename = String::new();
        for token in tokens {
            filename.push_str(&self.token_to_string(token, idents));
        }
        (filename, false)
    }

    /// Convert token to string
    fn token_to_string(&self, token: &Token, idents: &IdentTable) -> String {
        match &token.value {
            TokenValue::Ident(id) => idents.get_opt(*id).unwrap_or("").to_string(),
            TokenValue::Number(n) => n.clone(),
            TokenValue::String(s) => s.clone(),
            TokenValue::Special(code) => {
                if *code < 256 {
                    (*code as u8 as char).to_string()
                } else {
                    String::new()
                }
            }
            _ => String::new(),
        }
    }

    /// Find an include file
    /// Returns (IncludeSource, Option<system_include_path_index>)
    fn find_include_file(
        &self,
        filename: &str,
        is_system: bool,
        is_include_next: bool,
    ) -> Option<(IncludeSource, Option<usize>)> {
        // For #include_next, skip builtin headers entirely
        if !is_include_next {
            // Check builtin headers first (before any filesystem search)
            // Builtin headers take precedence for standard names like stdarg.h
            if self.use_builtin_headers {
                if let Some(content) = builtin_headers::get_builtin_header(filename) {
                    return Some((IncludeSource::Builtin(content), None));
                }
            }
        }

        // Absolute path
        if filename.starts_with('/') {
            let path = PathBuf::from(filename);
            if path.exists() {
                return Some((IncludeSource::File(path), None));
            }
            return None;
        }

        // For quoted includes (not #include_next), first check relative to current file
        if !is_system && !is_include_next {
            let relative_path = Path::new(&self.current_dir).join(filename);
            if relative_path.exists() {
                return Some((IncludeSource::File(relative_path), None));
            }
        }

        // Check -I include paths (for both quoted and angle bracket includes)
        // Per C standard, -I paths are searched for all includes
        if !is_include_next {
            for dir in &self.quote_include_paths {
                let path = Path::new(dir).join(filename);
                if path.exists() {
                    return Some((IncludeSource::File(path), None));
                }
            }
        }

        // Check system include paths (unless -nostdinc)
        if self.use_system_headers {
            // For #include_next, start from the path AFTER the current file's path
            let start_index = if is_include_next {
                self.current_include_path_index.map(|i| i + 1).unwrap_or(0)
            } else {
                0
            };

            for (idx, dir) in self
                .system_include_paths
                .iter()
                .enumerate()
                .skip(start_index)
            {
                let path = Path::new(dir).join(filename);
                if path.exists() {
                    return Some((IncludeSource::File(path), Some(idx)));
                }
            }
        }

        None
    }

    /// Include a file
    fn include_file(
        &mut self,
        path: &Path,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
        hash_token: &Token,
        include_path_index: Option<usize>,
    ) {
        // Canonicalize path for cycle detection
        let canonical = match path.canonicalize() {
            Ok(p) => p,
            Err(_) => path.to_path_buf(),
        };

        // Check for #pragma once
        if self.once_files.contains(&canonical) {
            return;
        }

        // Check for include cycle
        if self.include_stack.contains(&canonical) {
            diag::error(
                hash_token.pos,
                &format!("recursive include of '{}'", path.display()),
            );
            return;
        }

        // Check include depth
        if self.include_depth >= self.max_include_depth {
            diag::error(
                hash_token.pos,
                &format!(
                    "#include nested too deeply (max {})",
                    self.max_include_depth
                ),
            );
            return;
        }

        // Read the file
        let content = match fs::read(path) {
            Ok(c) => c,
            Err(e) => {
                diag::error(
                    hash_token.pos,
                    &format!("cannot read '{}': {}", path.display(), e),
                );
                return;
            }
        };

        // Save current state
        let saved_file =
            std::mem::replace(&mut self.current_file, path.to_string_lossy().to_string());
        let saved_dir = std::mem::replace(
            &mut self.current_dir,
            path.parent()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string()),
        );
        // Save cond_stack - included files have isolated conditional state
        let saved_cond_stack = std::mem::take(&mut self.cond_stack);
        // Save include path index for #include_next support
        let saved_include_path_index =
            std::mem::replace(&mut self.current_include_path_index, include_path_index);

        self.include_stack.insert(canonical.clone());
        self.include_depth += 1;

        // Create a new stream for this file
        let stream_id = diag::init_stream(&self.current_file);

        // Tokenize the included file using the same shared string table
        // Since we use the same StringTable, all StringIds are consistent
        // and no ID remapping is needed.
        let tokens = {
            let mut tokenizer = Tokenizer::new(&content, stream_id, idents);
            tokenizer.tokenize()
        };

        // Preprocess the included tokens
        let preprocessed = self.preprocess(tokens, idents);

        // Filter out stream markers from included content
        for token in preprocessed {
            match token.typ {
                TokenType::StreamBegin | TokenType::StreamEnd => {}
                _ => output.push(token),
            }
        }

        // Restore state
        self.include_depth -= 1;
        self.include_stack.remove(&canonical);
        self.current_file = saved_file;
        self.current_dir = saved_dir;
        self.cond_stack = saved_cond_stack;
        self.current_include_path_index = saved_include_path_index;
    }

    /// Include a builtin (embedded) header
    fn include_builtin(
        &mut self,
        name: &str,
        content: &str,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
        hash_token: &Token,
    ) {
        // Check include depth
        if self.include_depth >= self.max_include_depth {
            diag::error(
                hash_token.pos,
                &format!(
                    "#include nested too deeply (max {})",
                    self.max_include_depth
                ),
            );
            return;
        }

        // Save current state
        let saved_file = std::mem::replace(&mut self.current_file, format!("<builtin:{}>", name));
        let saved_dir = std::mem::replace(&mut self.current_dir, ".".to_string());
        let saved_cond_stack = std::mem::take(&mut self.cond_stack);

        self.include_depth += 1;

        // Create a stream for this builtin header
        let stream_id = diag::init_stream(&self.current_file);

        // Tokenize the builtin content
        let tokens = {
            let mut tokenizer = Tokenizer::new(content.as_bytes(), stream_id, idents);
            tokenizer.tokenize()
        };

        // Preprocess the included tokens
        let preprocessed = self.preprocess(tokens, idents);

        // Filter out stream markers from included content
        for token in preprocessed {
            match token.typ {
                TokenType::StreamBegin | TokenType::StreamEnd => {}
                _ => output.push(token),
            }
        }

        // Restore state
        self.include_depth -= 1;
        self.current_file = saved_file;
        self.current_dir = saved_dir;
        self.cond_stack = saved_cond_stack;
    }

    /// Handle #error
    fn handle_error<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        pos: &Position,
        idents: &IdentTable,
    ) where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        let tokens = self.collect_to_eol(iter);
        let msg = self.tokens_to_text(&tokens, idents);
        diag::error(*pos, &format!("#error {}", msg));
    }

    /// Handle #warning
    fn handle_warning<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        pos: &Position,
        idents: &IdentTable,
    ) where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        let tokens = self.collect_to_eol(iter);
        let msg = self.tokens_to_text(&tokens, idents);
        diag::warning(*pos, &format!("#warning {}", msg));
    }

    /// Handle #pragma
    fn handle_pragma<I>(&mut self, iter: &mut std::iter::Peekable<I>, idents: &IdentTable)
    where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Check for #pragma once
        if let Some(token) = iter.peek() {
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    if name == "once" {
                        if let Ok(canonical) = Path::new(&self.current_file).canonicalize() {
                            self.once_files.insert(canonical);
                        }
                    }
                }
            }
        }

        self.skip_to_eol(iter);
    }

    /// Handle _Pragma operator (C99)
    /// _Pragma("string") is equivalent to #pragma string
    /// Since we ignore most pragmas anyway, this just consumes the tokens
    fn handle_pragma_operator<I>(&mut self, iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = Token>,
    {
        // Expect '('
        if let Some(token) = iter.next() {
            if !matches!(&token.value, TokenValue::Special(code) if *code == b'(' as u32) {
                // Not a valid _Pragma - just silently ignore
                return;
            }
        } else {
            return;
        }

        // Expect a string literal
        if let Some(token) = iter.next() {
            if !matches!(token.typ, TokenType::String) {
                // Not a valid _Pragma - just silently ignore
                return;
            }
            // We could parse the pragma string here if needed
            // For now, we just ignore all pragmas
        } else {
            return;
        }

        // Expect ')' - if not found or malformed, silently ignore
        // (we've already consumed the tokens, so just return either way)
        if let Some(token) = iter.next() {
            if !matches!(&token.value, TokenValue::Special(code) if *code == b')' as u32) {
                // Not a valid _Pragma - silently ignored
            }
        }
        // Successfully consumed _Pragma("...")
    }

    /// Handle #line directive
    fn handle_line<I>(&mut self, iter: &mut std::iter::Peekable<I>)
    where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Just skip for now
        self.skip_to_eol(iter);
    }

    /// Convert tokens to text for error messages
    fn tokens_to_text(&self, tokens: &[Token], idents: &IdentTable) -> String {
        let mut result = String::new();
        for token in tokens {
            if !result.is_empty() && token.pos.whitespace {
                result.push(' ');
            }
            match &token.value {
                TokenValue::Ident(id) => {
                    if let Some(name) = idents.get_opt(*id) {
                        result.push_str(name);
                    }
                }
                TokenValue::Number(n) => result.push_str(n),
                TokenValue::String(s) => result.push_str(s),
                TokenValue::Special(code) if *code < 256 => {
                    result.push(*code as u8 as char);
                }
                _ => {}
            }
        }
        result
    }

    /// Try to expand a macro
    fn try_expand_macro<I>(
        &mut self,
        name: &str,
        pos: &Position,
        iter: &mut std::iter::Peekable<I>,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>>
    where
        I: Iterator<Item = Token>,
    {
        // Check for recursion
        if self.expanding.contains(name) {
            return None;
        }

        let mac = self.macros.get(name)?.clone();

        // Handle builtin macros
        if let Some(builtin) = mac.builtin {
            return self.expand_builtin(builtin, pos, &mac, iter, idents);
        }

        // Handle function-like macros
        if mac.is_function {
            // Check for opening paren
            if let Some(next) = iter.peek() {
                if let TokenValue::Special(code) = &next.value {
                    if *code == b'(' as u32 {
                        iter.next(); // consume '('
                        let args = self.collect_macro_args(iter, idents, pos, name);
                        return self.expand_function_macro(&mac, &args, pos, idents);
                    }
                }
            }
            // No paren - don't expand
            return None;
        }

        // Object-like macro
        self.expand_object_macro(&mac, pos, idents)
    }

    /// Collect arguments for a function-like macro call
    fn collect_macro_args<I>(
        &self,
        iter: &mut std::iter::Peekable<I>,
        _idents: &IdentTable,
        macro_pos: &Position,
        macro_name: &str,
    ) -> Vec<Vec<Token>>
    where
        I: Iterator<Item = Token>,
    {
        let mut args = Vec::new();
        let mut current_arg = Vec::new();
        // Start at depth 1 because the opening '(' has already been consumed
        // by the caller. This is important for handling multiline macro calls.
        let mut paren_depth = 1;
        let mut found_closing_paren = false;

        for token in iter.by_ref() {
            match &token.value {
                TokenValue::Special(code) => {
                    if *code == b'(' as u32 {
                        paren_depth += 1;
                        current_arg.push(token);
                    } else if *code == b')' as u32 {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            // End of arguments - closing ')' of macro call
                            if !current_arg.is_empty() || !args.is_empty() {
                                args.push(current_arg);
                            }
                            found_closing_paren = true;
                            break;
                        }
                        // Nested ')' - add to current argument
                        current_arg.push(token);
                    } else if *code == b',' as u32 && paren_depth == 1 {
                        // Argument separator at top level (paren_depth == 1 since we're inside the macro call)
                        args.push(current_arg);
                        current_arg = Vec::new();
                    } else {
                        current_arg.push(token);
                    }
                }
                _ => {
                    current_arg.push(token);
                }
            }
        }

        // Check for unterminated macro call (EOF before closing ')')
        if !found_closing_paren {
            crate::diag::error(
                *macro_pos,
                &format!(
                    "unterminated argument list invoking macro \"{}\"",
                    macro_name
                ),
            );
        }

        args
    }

    /// Expand a function-like macro
    fn expand_function_macro(
        &mut self,
        mac: &Macro,
        args: &[Vec<Token>],
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        self.expanding.insert(mac.name.clone());

        let mut result = Vec::new();
        let mut i = 0;

        while i < mac.body.len() {
            let mt = &mac.body[i];

            // Check for token pasting
            let next_is_paste =
                i + 1 < mac.body.len() && matches!(mac.body[i + 1].value, MacroTokenValue::Paste);
            let prev_was_paste = i > 0 && matches!(mac.body[i - 1].value, MacroTokenValue::Paste);

            match &mt.value {
                MacroTokenValue::Paste => {
                    // Skip paste markers
                    i += 1;
                    continue;
                }
                MacroTokenValue::Stringify(idx) => {
                    // Stringify the argument
                    let arg = args.get(*idx).cloned().unwrap_or_default();
                    let text = self.tokens_to_text(&arg, idents);
                    result.push(Token::with_value(
                        TokenType::String,
                        *pos,
                        TokenValue::String(text),
                    ));
                }
                MacroTokenValue::Param(idx) => {
                    let arg = args.get(*idx).cloned().unwrap_or_default();

                    if next_is_paste || prev_was_paste {
                        // Don't expand for token pasting
                        if prev_was_paste && !result.is_empty() {
                            // Paste with previous token
                            let prev = result.pop().unwrap();
                            let pasted = self.paste_tokens(&prev, &arg, pos, idents);
                            result.extend(pasted);
                        } else {
                            result.extend(arg);
                        }
                    } else {
                        // Expand the argument
                        let expanded = self.preprocess(arg, idents);
                        for mut tok in expanded {
                            if matches!(tok.typ, TokenType::StreamBegin | TokenType::StreamEnd) {
                                continue;
                            }
                            tok.pos = *pos;
                            tok.pos.newline = false;
                            result.push(tok);
                        }
                    }
                }
                MacroTokenValue::VaArgs => {
                    // Variadic arguments
                    let start = mac.params.len();
                    let va_args: Vec<_> = args.iter().skip(start).collect();
                    let va_args_empty =
                        va_args.is_empty() || (va_args.len() == 1 && va_args[0].is_empty());

                    // GNU extension: ,##__VA_ARGS__ - suppress comma when VA_ARGS is empty
                    if prev_was_paste && va_args_empty {
                        // Check if the previous token (before ##) was a comma and remove it
                        if let Some(last) = result.last() {
                            if let TokenValue::Special(code) = &last.value {
                                if *code == b',' as u32 {
                                    result.pop(); // Remove the comma
                                }
                            }
                        }
                        // Skip emitting empty VA_ARGS
                        i += 1;
                        continue;
                    }

                    for (j, arg) in args.iter().enumerate().skip(start) {
                        if j > start {
                            result.push(Token::with_value(
                                TokenType::Special,
                                *pos,
                                TokenValue::Special(b',' as u32),
                            ));
                        }
                        if next_is_paste || prev_was_paste {
                            result.extend(arg.clone());
                        } else {
                            let expanded = self.preprocess(arg.clone(), idents);
                            for mut tok in expanded {
                                if matches!(tok.typ, TokenType::StreamBegin | TokenType::StreamEnd)
                                {
                                    continue;
                                }
                                tok.pos = *pos;
                                tok.pos.newline = false;
                                result.push(tok);
                            }
                        }
                    }
                }
                _ => {
                    let token = self.macro_token_to_token(mt, pos, idents);

                    if prev_was_paste && !result.is_empty() {
                        // Paste with previous token
                        let prev = result.pop().unwrap();
                        let pasted = self.paste_tokens(&prev, &[token], pos, idents);
                        result.extend(pasted);
                    } else {
                        result.push(token);
                    }
                }
            }

            i += 1;
        }

        // Rescan for more macro expansion
        // NOTE: Keep macro in expanding set during rescan to prevent infinite recursion
        let rescanned = self.preprocess(result, idents);

        self.expanding.remove(&mac.name);

        let filtered: Vec<_> = rescanned
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .collect();

        Some(filtered)
    }

    /// Paste tokens together
    fn paste_tokens(
        &self,
        left: &Token,
        right: &[Token],
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Vec<Token> {
        if right.is_empty() {
            return vec![left.clone()];
        }

        let left_str = self.token_to_string(left, idents);
        let right_str = self.token_to_string(&right[0], idents);
        let combined = format!("{}{}", left_str, right_str);

        // Re-tokenize the combined string using the same shared string table
        // Since we use the same StringTable, all StringIds are consistent
        // and no ID remapping is needed.
        let stream_id = diag::init_stream("<paste>");
        let tokens = {
            let mut tokenizer = Tokenizer::new(combined.as_bytes(), stream_id, idents);
            tokenizer.tokenize()
        };

        let mut result: Vec<_> = tokens
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .map(|mut t| {
                t.pos = *pos;
                t.pos.newline = false;
                t
            })
            .collect();

        // Add remaining right tokens
        result.extend(right.iter().skip(1).cloned());

        result
    }

    /// Expand an object-like macro
    fn expand_object_macro(
        &mut self,
        mac: &Macro,
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        if mac.body.is_empty() {
            return Some(vec![]);
        }

        self.expanding.insert(mac.name.clone());

        let mut result = Vec::new();
        let mut i = 0;

        while i < mac.body.len() {
            let mt = &mac.body[i];

            // Check for token pasting - prev_was_paste means we need to paste with previous result
            let prev_was_paste = i > 0 && matches!(mac.body[i - 1].value, MacroTokenValue::Paste);

            match &mt.value {
                MacroTokenValue::Paste => {
                    // Skip paste markers
                    i += 1;
                    continue;
                }
                _ => {
                    let token = self.macro_token_to_token(mt, pos, idents);

                    if prev_was_paste && !result.is_empty() {
                        // Paste with previous token
                        let prev = result.pop().unwrap();
                        let pasted = self.paste_tokens(&prev, &[token], pos, idents);
                        result.extend(pasted);
                    } else {
                        result.push(token);
                    }
                }
            }

            i += 1;
        }

        // Rescan for more macro expansion
        // NOTE: Keep macro in expanding set during rescan to prevent infinite recursion
        // (e.g., when const -> __const and __const -> const both exist)
        let rescanned = self.preprocess(result, idents);

        self.expanding.remove(&mac.name);

        let filtered: Vec<_> = rescanned
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .collect();

        Some(filtered)
    }

    /// Convert a macro token to a regular token
    fn macro_token_to_token(
        &self,
        mt: &MacroToken,
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Token {
        let mut new_pos = *pos;
        new_pos.whitespace = mt.whitespace;
        new_pos.newline = false;

        let value = match &mt.value {
            MacroTokenValue::Number(n) => TokenValue::Number(n.clone()),
            MacroTokenValue::Ident(name) => {
                let id = idents.intern(name);
                TokenValue::Ident(id)
            }
            MacroTokenValue::String(s) => TokenValue::String(s.clone()),
            MacroTokenValue::Char(c) => TokenValue::Char(c.clone()),
            MacroTokenValue::Special(code) => TokenValue::Special(*code),
            _ => TokenValue::None,
        };

        Token::with_value(mt.typ, new_pos, value)
    }

    /// Expand a builtin macro
    fn expand_builtin<I>(
        &mut self,
        builtin: BuiltinMacro,
        pos: &Position,
        mac: &Macro,
        iter: &mut std::iter::Peekable<I>,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>>
    where
        I: Iterator<Item = Token>,
    {
        match builtin {
            BuiltinMacro::Line => Some(vec![Token::with_value(
                TokenType::Number,
                *pos,
                TokenValue::Number(pos.line.to_string()),
            )]),
            BuiltinMacro::File => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(self.current_file.clone()),
            )]),
            BuiltinMacro::Date => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(self.compile_date.clone()),
            )]),
            BuiltinMacro::Time => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(self.compile_time.clone()),
            )]),
            BuiltinMacro::Counter => {
                let val = self.counter;
                self.counter += 1;
                Some(vec![Token::with_value(
                    TokenType::Number,
                    *pos,
                    TokenValue::Number(val.to_string()),
                )])
            }
            BuiltinMacro::IncludeLevel => Some(vec![Token::with_value(
                TokenType::Number,
                *pos,
                TokenValue::Number(self.include_depth.to_string()),
            )]),
            BuiltinMacro::BaseFile => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(self.base_file.clone()),
            )]),
            BuiltinMacro::HasAttribute
            | BuiltinMacro::HasBuiltin
            | BuiltinMacro::HasFeature
            | BuiltinMacro::HasExtension => {
                // Consume the arguments
                if let Some(next) = iter.peek() {
                    if let TokenValue::Special(code) = &next.value {
                        if *code == b'(' as u32 {
                            iter.next();
                            let args = self.collect_macro_args(iter, idents, pos, &mac.name);
                            let result = self.eval_has_builtin(builtin, &args, idents);
                            return Some(vec![Token::with_value(
                                TokenType::Number,
                                *pos,
                                TokenValue::Number(if result { "1" } else { "0" }.to_string()),
                            )]);
                        }
                    }
                }
                Some(vec![Token::with_value(
                    TokenType::Number,
                    *pos,
                    TokenValue::Number("0".to_string()),
                )])
            }
            BuiltinMacro::HasInclude | BuiltinMacro::HasIncludeNext => {
                // Consume the arguments
                if let Some(next) = iter.peek() {
                    if let TokenValue::Special(code) = &next.value {
                        if *code == b'(' as u32 {
                            iter.next();
                            let args = self.collect_macro_args(iter, idents, pos, &mac.name);
                            let result = self.eval_has_include(&args, idents);
                            return Some(vec![Token::with_value(
                                TokenType::Number,
                                *pos,
                                TokenValue::Number(if result { "1" } else { "0" }.to_string()),
                            )]);
                        }
                    }
                }
                Some(vec![Token::with_value(
                    TokenType::Number,
                    *pos,
                    TokenValue::Number("0".to_string()),
                )])
            }
        }
    }

    /// Evaluate __has_attribute, __has_builtin, etc.
    fn eval_has_builtin(
        &self,
        builtin: BuiltinMacro,
        args: &[Vec<Token>],
        idents: &IdentTable,
    ) -> bool {
        if args.is_empty() {
            return false;
        }

        // Get the argument name
        let name = if let Some(tok) = args[0].first() {
            self.token_to_string(tok, idents)
        } else {
            return false;
        };

        match builtin {
            BuiltinMacro::HasAttribute => {
                // Return true for attributes we actually implement
                matches!(name.as_str(), "noreturn" | "__noreturn__")
            }
            BuiltinMacro::HasBuiltin => {
                // Return true only for builtins actually implemented in the compiler
                matches!(
                    name.as_str(),
                    // Variadic function support
                    "__builtin_va_list"
                        | "__builtin_va_start"
                        | "__builtin_va_end"
                        | "__builtin_va_arg"
                        | "__builtin_va_copy"
                        // Byte swap
                        | "__builtin_bswap16"
                        | "__builtin_bswap32"
                        | "__builtin_bswap64"
                        // Bit manipulation
                        | "__builtin_ctz"
                        | "__builtin_ctzl"
                        | "__builtin_ctzll"
                        | "__builtin_clz"
                        | "__builtin_clzl"
                        | "__builtin_clzll"
                        | "__builtin_popcount"
                        | "__builtin_popcountl"
                        | "__builtin_popcountll"
                        // Memory
                        | "__builtin_alloca"
                        // Compile-time evaluation
                        | "__builtin_constant_p"
                        | "__builtin_types_compatible_p"
                        | "__builtin_unreachable"
                        | "__builtin_offsetof"
                        | "offsetof"
                        // Floating-point constants
                        | "__builtin_inf"
                        | "__builtin_inff"
                        | "__builtin_infl"
                        | "__builtin_huge_val"
                        | "__builtin_huge_valf"
                        | "__builtin_huge_vall"
                        // Floating-point math
                        | "__builtin_fabs"
                        | "__builtin_fabsf"
                        | "__builtin_fabsl"
                        // Atomic builtins (C11 - Clang style)
                        | "__c11_atomic_init"
                        | "__c11_atomic_load"
                        | "__c11_atomic_store"
                        | "__c11_atomic_exchange"
                        | "__c11_atomic_compare_exchange_strong"
                        | "__c11_atomic_compare_exchange_weak"
                        | "__c11_atomic_fetch_add"
                        | "__c11_atomic_fetch_sub"
                        | "__c11_atomic_fetch_and"
                        | "__c11_atomic_fetch_or"
                        | "__c11_atomic_fetch_xor"
                        | "__c11_atomic_thread_fence"
                        | "__c11_atomic_signal_fence"
                )
            }
            BuiltinMacro::HasFeature => {
                // Return true for GNU extensions we implement
                matches!(name.as_str(), "statement_expressions")
            }
            BuiltinMacro::HasExtension => {
                // Return true for GNU extensions we implement
                matches!(name.as_str(), "statement_expressions")
            }
            _ => false,
        }
    }

    /// Evaluate __has_include
    fn eval_has_include(&self, args: &[Vec<Token>], idents: &IdentTable) -> bool {
        if args.is_empty() {
            return false;
        }

        let (filename, is_system) = self.parse_include_path(&args[0], idents);
        self.find_include_file(&filename, is_system, false)
            .is_some()
    }
}

// ============================================================================
// Expression Evaluator for #if
// ============================================================================

struct ExprEvaluator<'a, 'b> {
    pp: &'a Preprocessor<'b>,
    idents: &'a IdentTable,
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a, 'b> ExprEvaluator<'a, 'b> {
    fn new(pp: &'a Preprocessor<'b>, idents: &'a IdentTable) -> Self {
        Self {
            pp,
            idents,
            tokens: Vec::new(),
            pos: 0,
        }
    }

    fn evaluate(&mut self, tokens: &[Token]) -> i64 {
        self.tokens = tokens.to_vec();
        self.pos = 0;
        self.expr_ternary()
    }

    /// Ternary operator has lowest precedence: cond ? true_val : false_val
    fn expr_ternary(&mut self) -> i64 {
        let cond = self.expr_or();
        if self.is_special(b'?' as u32) {
            self.advance();
            let true_val = self.expr_ternary();
            if self.is_special(b':' as u32) {
                self.advance();
            } else {
                let pos = self.current().map(|t| t.pos).unwrap_or_default();
                diag::error(pos, "expected ':' in conditional expression");
            }
            let false_val = self.expr_ternary();
            if cond != 0 {
                true_val
            } else {
                false_val
            }
        } else {
            cond
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn is_special(&self, expected: u32) -> bool {
        if let Some(tok) = self.current() {
            if let TokenValue::Special(code) = &tok.value {
                return *code == expected;
            }
        }
        false
    }

    fn is_ident(&self, expected: &str) -> bool {
        if let Some(tok) = self.current() {
            if let TokenValue::Ident(id) = &tok.value {
                if let Some(name) = self.idents.get_opt(*id) {
                    return name == expected;
                }
            }
        }
        false
    }

    fn get_ident(&self) -> Option<String> {
        if let Some(tok) = self.current() {
            if let TokenValue::Ident(id) = &tok.value {
                return self.idents.get_opt(*id).map(|s| s.to_string());
            }
        }
        None
    }

    // Operator precedence (lowest to highest):
    // ||
    // &&
    // |
    // ^
    // &
    // == !=
    // < <= > >=
    // << >>
    // + -
    // * / %
    // ! ~ - + (unary)
    // defined, primary

    fn expr_or(&mut self) -> i64 {
        let mut left = self.expr_and();
        while self.is_special(SpecialToken::LogicalOr as u32) {
            self.advance();
            let right = self.expr_and();
            left = if left != 0 || right != 0 { 1 } else { 0 };
        }
        left
    }

    fn expr_and(&mut self) -> i64 {
        let mut left = self.expr_bitor();
        while self.is_special(SpecialToken::LogicalAnd as u32) {
            self.advance();
            let right = self.expr_bitor();
            left = if left != 0 && right != 0 { 1 } else { 0 };
        }
        left
    }

    fn expr_bitor(&mut self) -> i64 {
        let mut left = self.expr_bitxor();
        while self.is_special(b'|' as u32) {
            self.advance();
            let right = self.expr_bitxor();
            left |= right;
        }
        left
    }

    fn expr_bitxor(&mut self) -> i64 {
        let mut left = self.expr_bitand();
        while self.is_special(b'^' as u32) {
            self.advance();
            let right = self.expr_bitand();
            left ^= right;
        }
        left
    }

    fn expr_bitand(&mut self) -> i64 {
        let mut left = self.expr_equality();
        while self.is_special(b'&' as u32) {
            self.advance();
            let right = self.expr_equality();
            left &= right;
        }
        left
    }

    fn expr_equality(&mut self) -> i64 {
        let mut left = self.expr_relational();
        loop {
            if self.is_special(SpecialToken::Equal as u32) {
                self.advance();
                let right = self.expr_relational();
                left = if left == right { 1 } else { 0 };
            } else if self.is_special(SpecialToken::NotEqual as u32) {
                self.advance();
                let right = self.expr_relational();
                left = if left != right { 1 } else { 0 };
            } else {
                break;
            }
        }
        left
    }

    fn expr_relational(&mut self) -> i64 {
        let mut left = self.expr_shift();
        loop {
            if self.is_special(b'<' as u32) {
                self.advance();
                let right = self.expr_shift();
                left = if left < right { 1 } else { 0 };
            } else if self.is_special(b'>' as u32) {
                self.advance();
                let right = self.expr_shift();
                left = if left > right { 1 } else { 0 };
            } else if self.is_special(SpecialToken::Lte as u32) {
                self.advance();
                let right = self.expr_shift();
                left = if left <= right { 1 } else { 0 };
            } else if self.is_special(SpecialToken::Gte as u32) {
                self.advance();
                let right = self.expr_shift();
                left = if left >= right { 1 } else { 0 };
            } else {
                break;
            }
        }
        left
    }

    fn expr_shift(&mut self) -> i64 {
        let mut left = self.expr_additive();
        loop {
            if self.is_special(SpecialToken::LeftShift as u32) {
                self.advance();
                let right = self.expr_additive();
                left <<= right;
            } else if self.is_special(SpecialToken::RightShift as u32) {
                self.advance();
                let right = self.expr_additive();
                left >>= right;
            } else {
                break;
            }
        }
        left
    }

    fn expr_additive(&mut self) -> i64 {
        let mut left = self.expr_multiplicative();
        loop {
            if self.is_special(b'+' as u32) {
                self.advance();
                let right = self.expr_multiplicative();
                left += right;
            } else if self.is_special(b'-' as u32) {
                self.advance();
                let right = self.expr_multiplicative();
                left -= right;
            } else {
                break;
            }
        }
        left
    }

    fn expr_multiplicative(&mut self) -> i64 {
        let mut left = self.expr_unary();
        loop {
            if self.is_special(b'*' as u32) {
                self.advance();
                let right = self.expr_unary();
                left *= right;
            } else if self.is_special(b'/' as u32) {
                self.advance();
                let right = self.expr_unary();
                if right != 0 {
                    left /= right;
                }
            } else if self.is_special(b'%' as u32) {
                self.advance();
                let right = self.expr_unary();
                if right != 0 {
                    left %= right;
                }
            } else {
                break;
            }
        }
        left
    }

    fn expr_unary(&mut self) -> i64 {
        if self.is_special(b'!' as u32) {
            self.advance();
            let val = self.expr_unary();
            return if val == 0 { 1 } else { 0 };
        }
        if self.is_special(b'~' as u32) {
            self.advance();
            let val = self.expr_unary();
            return !val;
        }
        if self.is_special(b'-' as u32) {
            self.advance();
            let val = self.expr_unary();
            return -val;
        }
        if self.is_special(b'+' as u32) {
            self.advance();
            return self.expr_unary();
        }
        self.expr_primary()
    }

    fn expr_primary(&mut self) -> i64 {
        // Handle defined(X) or defined X
        if self.is_ident("defined") {
            self.advance();
            return self.eval_defined();
        }

        // Handle __has_attribute(X)
        if self.is_ident("__has_attribute") {
            self.advance();
            return self.eval_has_attribute();
        }

        // Handle __has_builtin(X)
        if self.is_ident("__has_builtin") {
            self.advance();
            return self.eval_has_builtin_expr();
        }

        // Handle __has_feature(X) and __has_extension(X)
        if self.is_ident("__has_feature") || self.is_ident("__has_extension") {
            self.advance();
            return self.eval_has_feature();
        }

        // Handle parenthesized expression
        if self.is_special(b'(' as u32) {
            self.advance();
            let val = self.expr_or();
            if self.is_special(b')' as u32) {
                self.advance();
            }
            return val;
        }

        // Handle number
        if let Some(tok) = self.current() {
            if let TokenValue::Number(n) = &tok.value {
                let num_str = n.clone();
                self.advance();
                return self.parse_number(&num_str);
            }
        }

        // Handle character literal
        if let Some(tok) = self.current() {
            if let TokenValue::Char(c) = &tok.value {
                let char_str = c.clone();
                self.advance();
                if char_str.is_empty() {
                    return 0;
                }
                return char_str.chars().next().unwrap_or('\0') as i64;
            }
        }

        // Handle identifier - after macro expansion, any remaining identifier
        // is undefined and should evaluate to 0 (per C standard)
        if let Some(tok) = self.current() {
            if matches!(&tok.value, TokenValue::Ident(_)) {
                self.advance();
                return 0;
            }
        }

        0
    }

    fn eval_defined(&mut self) -> i64 {
        // defined(X) or defined X
        let mut need_paren = false;
        if self.is_special(b'(' as u32) {
            self.advance();
            need_paren = true;
        }

        let result = if let Some(name) = self.get_ident() {
            self.advance();
            if self.pp.is_defined(&name) {
                1
            } else {
                0
            }
        } else {
            0
        };

        if need_paren && self.is_special(b')' as u32) {
            self.advance();
        }

        result
    }

    /// Get the identifier argument from a __has_* expression: __has_*(ident)
    fn get_has_arg(&mut self) -> Option<String> {
        if !self.is_special(b'(' as u32) {
            return None;
        }
        self.advance(); // consume '('

        let name = self.get_ident();
        if name.is_some() {
            self.advance(); // consume identifier
        }

        if self.is_special(b')' as u32) {
            self.advance(); // consume ')'
        }

        name
    }

    /// Evaluate __has_attribute(X)
    fn eval_has_attribute(&mut self) -> i64 {
        let name = match self.get_has_arg() {
            Some(n) => n,
            None => return 0,
        };

        // Return 1 for attributes we actually implement
        let supported = matches!(name.as_str(), "noreturn" | "__noreturn__");
        if supported {
            1
        } else {
            0
        }
    }

    /// Evaluate __has_builtin(X)
    fn eval_has_builtin_expr(&mut self) -> i64 {
        let name = match self.get_has_arg() {
            Some(n) => n,
            None => return 0,
        };

        // Return 1 for builtins actually implemented in the compiler
        let supported = matches!(
            name.as_str(),
            // Variadic function support
            "__builtin_va_list"
                | "__builtin_va_start"
                | "__builtin_va_end"
                | "__builtin_va_arg"
                | "__builtin_va_copy"
                // Byte swap
                | "__builtin_bswap16"
                | "__builtin_bswap32"
                | "__builtin_bswap64"
                // Bit manipulation
                | "__builtin_ctz"
                | "__builtin_ctzl"
                | "__builtin_ctzll"
                | "__builtin_clz"
                | "__builtin_clzl"
                | "__builtin_clzll"
                | "__builtin_popcount"
                | "__builtin_popcountl"
                | "__builtin_popcountll"
                // Memory
                | "__builtin_alloca"
                // Compile-time evaluation
                | "__builtin_constant_p"
                | "__builtin_types_compatible_p"
                | "__builtin_unreachable"
                | "__builtin_offsetof"
                | "offsetof"
                // Floating-point constants
                | "__builtin_inf"
                | "__builtin_inff"
                | "__builtin_infl"
                | "__builtin_huge_val"
                | "__builtin_huge_valf"
                | "__builtin_huge_vall"
                // Floating-point math
                | "__builtin_fabs"
                | "__builtin_fabsf"
                | "__builtin_fabsl"
                // Atomic builtins (C11 - Clang style)
                | "__c11_atomic_init"
                | "__c11_atomic_load"
                | "__c11_atomic_store"
                | "__c11_atomic_exchange"
                | "__c11_atomic_compare_exchange_strong"
                | "__c11_atomic_compare_exchange_weak"
                | "__c11_atomic_fetch_add"
                | "__c11_atomic_fetch_sub"
                | "__c11_atomic_fetch_and"
                | "__c11_atomic_fetch_or"
                | "__c11_atomic_fetch_xor"
                | "__c11_atomic_thread_fence"
                | "__c11_atomic_signal_fence"
        );

        if supported {
            1
        } else {
            0
        }
    }

    /// Evaluate __has_feature(X) and __has_extension(X)
    fn eval_has_feature(&mut self) -> i64 {
        let name = match self.get_has_arg() {
            Some(n) => n,
            None => return 0,
        };

        // Return 1 for GNU extensions we implement
        let supported = matches!(
            name.as_str(),
            "statement_expressions" // GNU ({ }) extension
        );

        if supported {
            1
        } else {
            0
        }
    }

    fn parse_number(&self, s: &str) -> i64 {
        let s = s.trim_end_matches(['u', 'U', 'l', 'L']);

        if s.starts_with("0x") || s.starts_with("0X") {
            i64::from_str_radix(&s[2..], 16).unwrap_or(0)
        } else if s.starts_with("0b") || s.starts_with("0B") {
            i64::from_str_radix(&s[2..], 2).unwrap_or(0)
        } else if s.starts_with('0') && s.len() > 1 && s.chars().nth(1).unwrap().is_ascii_digit() {
            i64::from_str_radix(&s[1..], 8).unwrap_or(0)
        } else {
            s.parse().unwrap_or(0)
        }
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Configuration for preprocessing command-line options
#[derive(Default)]
pub struct PreprocessConfig<'a> {
    /// Command-line -D defines
    pub defines: &'a [String],
    /// Command-line -U undefines
    pub undefines: &'a [String],
    /// Command-line -I include paths
    pub include_paths: &'a [String],
    /// If true, disable system include paths (-nostdinc)
    pub no_std_inc: bool,
    /// If true, disable builtin headers (-nobuiltininc)
    pub no_builtin_inc: bool,
}

/// Preprocess tokens with command-line defines and undefines
///
/// This is the main entry point for preprocessing.
/// Takes lexer output and returns preprocessed tokens.
///
/// # Arguments
/// * `tokens` - Lexer output tokens
/// * `target` - Target platform configuration
/// * `idents` - Identifier table for string interning
/// * `filename` - Name of the source file
/// * `config` - Preprocessing configuration (defines, undefines, include paths, flags)
pub fn preprocess_with_defines(
    tokens: Vec<Token>,
    target: &Target,
    idents: &mut IdentTable,
    filename: &str,
    config: &PreprocessConfig<'_>,
) -> Vec<Token> {
    let mut pp = Preprocessor::new(target, filename);

    // Handle -nostdinc and -nobuiltininc flags
    if config.no_std_inc {
        pp.use_system_headers = false;
        pp.use_builtin_headers = false;
    }
    if config.no_builtin_inc {
        pp.use_builtin_headers = false;
    }

    // Add -I include paths
    for path in config.include_paths {
        pp.quote_include_paths.push(path.clone());
    }

    // Process -D defines
    for def in config.defines {
        if let Some(eq_pos) = def.find('=') {
            // -DNAME=VALUE - tokenize the value properly
            let name = &def[..eq_pos];
            let value = &def[eq_pos + 1..];
            let mac = Macro::from_cmdline_define(name, value);
            pp.define_macro(mac);
        } else {
            // -DNAME (define to 1)
            let mac = Macro::predefined(def, Some("1"));
            pp.define_macro(mac);
        }
    }

    // Process -U undefines
    for undef in config.undefines {
        pp.undef_macro(undef);
    }

    pp.preprocess(tokens, idents)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::lexer::Tokenizer;

    fn preprocess_str(input: &str) -> (Vec<Token>, IdentTable) {
        let target = Target::host();
        let mut strings = IdentTable::new();
        let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
        let tokens = tokenizer.tokenize();
        let result = preprocess_with_defines(
            tokens,
            &target,
            &mut strings,
            "<test>",
            &PreprocessConfig::default(),
        );
        (result, strings)
    }

    fn get_token_strings(tokens: &[Token], idents: &IdentTable) -> Vec<String> {
        tokens
            .iter()
            .filter_map(|t| match &t.typ {
                TokenType::Ident => {
                    if let TokenValue::Ident(id) = &t.value {
                        idents.get_opt(*id).map(|s| s.to_string())
                    } else {
                        None
                    }
                }
                TokenType::Number => {
                    if let TokenValue::Number(n) = &t.value {
                        Some(n.clone())
                    } else {
                        None
                    }
                }
                TokenType::String => {
                    if let TokenValue::String(s) = &t.value {
                        Some(format!("\"{}\"", s))
                    } else {
                        None
                    }
                }
                TokenType::Special => {
                    if let TokenValue::Special(code) = &t.value {
                        if *code < 256 {
                            Some((*code as u8 as char).to_string())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }

    #[test]
    fn test_simple_define() {
        let (tokens, idents) = preprocess_str("#define FOO 42\nFOO");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"42".to_string()));
    }

    #[test]
    fn test_undef() {
        let (tokens, idents) = preprocess_str("#define FOO 42\n#undef FOO\nFOO");
        let strs = get_token_strings(&tokens, &idents);
        // FOO should not be expanded after undef
        assert!(strs.contains(&"FOO".to_string()));
    }

    #[test]
    fn test_ifdef_true() {
        let (tokens, idents) = preprocess_str("#define FOO\n#ifdef FOO\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_ifdef_false() {
        let (tokens, idents) = preprocess_str("#ifdef FOO\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_ifndef_true() {
        let (tokens, idents) = preprocess_str("#ifndef FOO\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_ifndef_false() {
        let (tokens, idents) = preprocess_str("#define FOO\n#ifndef FOO\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_ifdef_else() {
        let (tokens, idents) = preprocess_str("#ifdef FOO\nyes\n#else\nno\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_nested_ifdef() {
        let (tokens, idents) =
            preprocess_str("#define A\n#ifdef A\n#ifdef B\ninner\n#endif\nouter\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"inner".to_string())); // B not defined
        assert!(strs.contains(&"outer".to_string())); // A is defined
    }

    #[test]
    fn test_if_true() {
        let (tokens, idents) = preprocess_str("#if 1\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_false() {
        let (tokens, idents) = preprocess_str("#if 0\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_if_defined() {
        let (tokens, idents) = preprocess_str("#define FOO\n#if defined(FOO)\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_elif() {
        let (tokens, idents) = preprocess_str("#if 0\none\n#elif 1\ntwo\n#else\nthree\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"one".to_string()));
        assert!(strs.contains(&"two".to_string()));
        assert!(!strs.contains(&"three".to_string()));
    }

    #[test]
    fn test_predefined_stdc() {
        let target = Target::host();
        let pp = Preprocessor::new(&target, "test.c");
        assert!(pp.is_defined("__STDC__"));
        assert!(pp.is_defined("__STDC_VERSION__"));
    }

    #[test]
    fn test_predefined_arch() {
        let target = Target::host();
        let pp = Preprocessor::new(&target, "test.c");

        // Should have either x86_64 or aarch64 defined
        assert!(pp.is_defined("__x86_64__") || pp.is_defined("__aarch64__"));
    }

    #[test]
    fn test_line_macro() {
        let (tokens, _idents) = preprocess_str("__LINE__");
        // Should have a number token
        assert!(tokens.iter().any(|t| t.typ == TokenType::Number));
    }

    #[test]
    fn test_counter_macro() {
        let (tokens, _idents) = preprocess_str("__COUNTER__ __COUNTER__ __COUNTER__");
        let nums: Vec<_> = tokens
            .iter()
            .filter_map(|t| {
                if let TokenValue::Number(n) = &t.value {
                    Some(n.clone())
                } else {
                    None
                }
            })
            .collect();
        // Should have 0, 1, 2
        assert_eq!(nums, vec!["0", "1", "2"]);
    }

    #[test]
    fn test_deeply_nested_conditionals() {
        let input = r#"
#define A
#ifdef A
    level1
    #ifdef B
        level2a
    #else
        level2b
        #ifdef A
            level3
        #endif
    #endif
#endif
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);

        assert!(strs.contains(&"level1".to_string()));
        assert!(!strs.contains(&"level2a".to_string())); // B not defined
        assert!(strs.contains(&"level2b".to_string())); // else branch
        assert!(strs.contains(&"level3".to_string())); // A still defined
    }

    #[test]
    fn test_else_basic() {
        // Ensure #else works correctly when condition is false
        let (tokens, idents) = preprocess_str("#if 0\nyes\n#else\nno\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_endif_basic() {
        // Ensure #endif properly closes conditional blocks
        let (tokens, idents) = preprocess_str("#ifdef FOO\nskipped\n#endif\nafter");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"skipped".to_string()));
        assert!(strs.contains(&"after".to_string()));
    }

    #[test]
    fn test_include_skipped_in_false_branch() {
        // #include in a false branch should be skipped
        let (tokens, idents) = preprocess_str("#if 0\n#include <stdio.h>\n#endif\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
        // No error from trying to include stdio.h
    }

    #[test]
    fn test_error_skipped_in_false_branch() {
        // #error in a false branch should not trigger
        let (tokens, idents) =
            preprocess_str("#if 0\n#error This should not trigger\n#endif\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_warning_skipped_in_false_branch() {
        // #warning in a false branch should not trigger
        let (tokens, idents) =
            preprocess_str("#if 0\n#warning This should not trigger\n#endif\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_pragma_ignored() {
        // #pragma should be silently ignored
        let (tokens, idents) = preprocess_str("#pragma once\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_line_directive_ignored() {
        // #line should be processed (currently ignored)
        let (tokens, idents) = preprocess_str("#line 100\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_define_with_value() {
        // Test #define with a specific value
        let (tokens, idents) = preprocess_str("#define VALUE 123\nVALUE");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"123".to_string()));
    }

    #[test]
    fn test_define_empty() {
        // Test #define without value (flag-style macro)
        let (tokens, idents) = preprocess_str("#define FLAG\n#ifdef FLAG\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_undef_removes_macro() {
        // Verify #undef removes a macro so #ifdef fails
        let (tokens, idents) =
            preprocess_str("#define FOO\n#undef FOO\n#ifdef FOO\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_function_like_macro() {
        let (tokens, idents) = preprocess_str("#define ADD(a, b) a + b\nADD(1, 2)");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"1".to_string()));
        assert!(strs.contains(&"+".to_string()));
        assert!(strs.contains(&"2".to_string()));
    }

    #[test]
    fn test_if_logical_and() {
        let (tokens, idents) = preprocess_str("#if 1 && 1\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));

        let (tokens, idents) = preprocess_str("#if 1 && 0\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_if_logical_or() {
        let (tokens, idents) = preprocess_str("#if 0 || 1\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));

        let (tokens, idents) = preprocess_str("#if 0 || 0\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_if_not() {
        let (tokens, idents) = preprocess_str("#if !0\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));

        let (tokens, idents) = preprocess_str("#if !1\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_if_comparison() {
        let (tokens, idents) = preprocess_str("#if 5 > 3\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));

        let (tokens, idents) = preprocess_str("#if 5 < 3\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    // ========================================================================
    // Header guard tests
    // ========================================================================

    #[test]
    fn test_header_guard_basic() {
        // Simulates typical header guard pattern
        let input = r#"
#ifndef MY_HEADER_H
#define MY_HEADER_H
first_include
#endif
#ifndef MY_HEADER_H
#define MY_HEADER_H
second_include
#endif
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"first_include".to_string()));
        assert!(!strs.contains(&"second_include".to_string()));
    }

    #[test]
    fn test_header_guard_ifdef_style() {
        // Alternative header guard using #ifdef
        let input = r#"
#ifdef GUARD
#else
#define GUARD
first
#endif
#ifdef GUARD
second
#endif
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"first".to_string()));
        assert!(strs.contains(&"second".to_string()));
    }

    // ========================================================================
    // Multiple elif chain tests
    // ========================================================================

    #[test]
    fn test_multiple_elif_first() {
        let input = "#if 1\none\n#elif 1\ntwo\n#elif 1\nthree\n#else\nfour\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"one".to_string()));
        assert!(!strs.contains(&"two".to_string()));
        assert!(!strs.contains(&"three".to_string()));
        assert!(!strs.contains(&"four".to_string()));
    }

    #[test]
    fn test_multiple_elif_middle() {
        let input = "#if 0\none\n#elif 0\ntwo\n#elif 1\nthree\n#else\nfour\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"one".to_string()));
        assert!(!strs.contains(&"two".to_string()));
        assert!(strs.contains(&"three".to_string()));
        assert!(!strs.contains(&"four".to_string()));
    }

    #[test]
    fn test_multiple_elif_else() {
        let input = "#if 0\none\n#elif 0\ntwo\n#elif 0\nthree\n#else\nfour\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"one".to_string()));
        assert!(!strs.contains(&"two".to_string()));
        assert!(!strs.contains(&"three".to_string()));
        assert!(strs.contains(&"four".to_string()));
    }

    // ========================================================================
    // Defined operator tests
    // ========================================================================

    #[test]
    fn test_defined_without_parens() {
        let input = "#define FOO\n#if defined FOO\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_defined_not_defined() {
        let input = "#if defined(BAR)\nyes\n#endif\nno";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_defined_negated() {
        let input = "#if !defined(FOO)\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_defined_in_complex_expr() {
        let input = "#define A\n#if defined(A) && !defined(B)\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // ========================================================================
    // Macro expansion tests
    // ========================================================================

    #[test]
    fn test_multi_token_macro() {
        let input = "#define EXPR 1 + 2 + 3\nEXPR";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"1".to_string()));
        assert!(strs.contains(&"2".to_string()));
        assert!(strs.contains(&"3".to_string()));
    }

    #[test]
    fn test_nested_macro_expansion() {
        let input = "#define A B\n#define B 42\nA";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"42".to_string()));
    }

    #[test]
    fn test_macro_in_if_expr() {
        let input = "#define VAL 5\n#if VAL > 3\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_macro_redefinition() {
        // Macro redefinition should use latest value
        let input = "#define X 1\n#define X 2\nX";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"2".to_string()));
        assert!(!strs.contains(&"1".to_string()));
    }

    // ========================================================================
    // Arithmetic in #if expressions
    // ========================================================================

    #[test]
    fn test_if_addition() {
        let input = "#if 2 + 3 == 5\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_subtraction() {
        let input = "#if 10 - 3 == 7\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_multiplication() {
        let input = "#if 3 * 4 == 12\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_division() {
        let input = "#if 12 / 4 == 3\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_modulo() {
        let input = "#if 10 % 3 == 1\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_parentheses() {
        let input = "#if (2 + 3) * 2 == 10\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // ========================================================================
    // Comparison operators in #if
    // ========================================================================

    #[test]
    fn test_if_equal() {
        let input = "#if 5 == 5\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_not_equal() {
        let input = "#if 5 != 3\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_less_equal() {
        let input = "#if 3 <= 3\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_greater_equal() {
        let input = "#if 5 >= 5\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // ========================================================================
    // Bitwise operators in #if
    // ========================================================================

    #[test]
    fn test_if_bitwise_and() {
        let input = "#if 0xFF & 0x0F == 0x0F\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_if_bitwise_or() {
        let input = "#if (0xF0 | 0x0F) == 0xFF\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // ========================================================================
    // Edge cases
    // ========================================================================

    #[test]
    fn test_empty_if_block() {
        let input = "#if 1\n#endif\nafter";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"after".to_string()));
    }

    #[test]
    fn test_empty_else_block() {
        let input = "#if 0\nskipped\n#else\n#endif\nafter";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"skipped".to_string()));
        assert!(strs.contains(&"after".to_string()));
    }

    #[test]
    fn test_consecutive_conditionals() {
        let input = "#if 1\nfirst\n#endif\n#if 1\nsecond\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"first".to_string()));
        assert!(strs.contains(&"second".to_string()));
    }

    #[test]
    fn test_undefined_macro_is_zero() {
        // Undefined macros evaluate to 0 in #if expressions
        let input = "#if UNDEFINED\nyes\n#endif\nno";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_ternary_in_if() {
        let input = "#if 1 ? 1 : 0\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // Tests for nested conditional skipping (bug fix)
    #[test]
    fn test_nested_if_else_in_skipped_block() {
        // When outer #ifndef is false (guard defined), inner #if/#else should not activate
        let input = r#"
#define GUARD
#ifndef GUARD
outer_skipped
#if 0
inner_if_skipped
#else
inner_else_should_also_skip
#endif
#endif
after
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"outer_skipped".to_string()));
        assert!(!strs.contains(&"inner_if_skipped".to_string()));
        assert!(!strs.contains(&"inner_else_should_also_skip".to_string()));
        assert!(strs.contains(&"after".to_string()));
    }

    #[test]
    fn test_nested_elif_in_skipped_block() {
        // When outer block is skipped, nested #elif should not activate
        let input = r#"
#define GUARD
#ifndef GUARD
#if 0
a
#elif 1
b_should_not_appear
#else
c
#endif
#endif
done
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"a".to_string()));
        assert!(!strs.contains(&"b_should_not_appear".to_string()));
        assert!(!strs.contains(&"c".to_string()));
        assert!(strs.contains(&"done".to_string()));
    }

    #[test]
    fn test_deeply_nested_skipped_conditionals() {
        // Multiple levels of nesting inside a skipped block
        let input = r#"
#if 0
level1
#if 1
level2_should_skip
#if 1
level3_should_skip
#else
level3_else_should_skip
#endif
#endif
#endif
visible
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"level1".to_string()));
        assert!(!strs.contains(&"level2_should_skip".to_string()));
        assert!(!strs.contains(&"level3_should_skip".to_string()));
        assert!(!strs.contains(&"level3_else_should_skip".to_string()));
        assert!(strs.contains(&"visible".to_string()));
    }

    // Tests for token pasting in object-like macros (bug fix)
    #[test]
    fn test_token_paste_object_macro() {
        let input = "#define CONCAT a ## b\nCONCAT";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"ab".to_string()));
    }

    #[test]
    fn test_token_paste_object_macro_numbers() {
        let input = "#define NUM 1 ## 2 ## 3\nNUM";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"123".to_string()));
    }

    #[test]
    fn test_token_paste_object_macro_mixed() {
        let input = "#define PREFIX foo ## 123\nPREFIX";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"foo123".to_string()));
    }

    // Tests for token pasting in function-like macros
    #[test]
    fn test_token_paste_function_macro() {
        let input = "#define CONCAT(a, b) a ## b\nCONCAT(foo, bar)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"foobar".to_string()));
    }

    #[test]
    fn test_token_paste_function_macro_prefix() {
        let input = "#define MAKE_ID(x) id_ ## x\nMAKE_ID(test)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"id_test".to_string()));
    }

    #[test]
    fn test_token_paste_function_macro_suffix() {
        let input = "#define MAKE_FUNC(x) x ## _func\nMAKE_FUNC(my)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"my_func".to_string()));
    }

    #[test]
    fn test_token_paste_creates_identifier() {
        // Pasting should create a new identifier that can be used
        let input = r#"
#define PASTE(a, b) a ## b
#define foobar 42
PASTE(foo, bar)
"#;
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        // foobar should expand to 42
        assert!(strs.contains(&"42".to_string()));
    }

    // ========================================================================
    // Tests for __INCLUDE_LEVEL__ macro
    // ========================================================================

    #[test]
    fn test_include_level_macro() {
        // At the top level, __INCLUDE_LEVEL__ should be 0
        let (tokens, _idents) = preprocess_str("__INCLUDE_LEVEL__");
        let nums: Vec<_> = tokens
            .iter()
            .filter_map(|t| {
                if let TokenValue::Number(n) = &t.value {
                    Some(n.clone())
                } else {
                    None
                }
            })
            .collect();
        assert!(nums.contains(&"0".to_string()));
    }

    // ========================================================================
    // Tests for __BASE_FILE__ macro
    // ========================================================================

    #[test]
    fn test_base_file_macro() {
        // __BASE_FILE__ should return the base filename
        let (tokens, _idents) = preprocess_str("__BASE_FILE__");
        // Should have a string token
        assert!(tokens.iter().any(|t| t.typ == TokenType::String));
    }

    // ========================================================================
    // Tests for ternary operator in #if expressions
    // ========================================================================

    #[test]
    fn test_ternary_true_branch() {
        let (tokens, idents) = preprocess_str("#if 1 ? 1 : 0\nyes\n#endif");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_ternary_false_branch() {
        let (tokens, idents) = preprocess_str("#if 0 ? 1 : 0\nyes\n#endif\nno");
        let strs = get_token_strings(&tokens, &idents);
        assert!(!strs.contains(&"yes".to_string()));
        assert!(strs.contains(&"no".to_string()));
    }

    #[test]
    fn test_ternary_nested() {
        // Nested ternary: 1 ? (0 ? 1 : 2) : 3 = 2
        let input = "#if (1 ? (0 ? 1 : 2) : 3) == 2\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_ternary_with_expressions() {
        // ((5 > 3) ? 10 : 20) == 10 = 1 (true)
        let input = "#if ((5 > 3) ? 10 : 20) == 10\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    #[test]
    fn test_ternary_with_defined() {
        let input = "#define FOO\n#if defined(FOO) ? 1 : 0\nyes\n#endif";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"yes".to_string()));
    }

    // ========================================================================
    // Tests for GNU ,##__VA_ARGS__ comma suppression
    // ========================================================================

    #[test]
    fn test_va_args_basic() {
        // Basic variadic macro with arguments
        let input = "#define DEBUG(fmt, ...) fmt __VA_ARGS__\nDEBUG(hello, world)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"hello".to_string()));
        assert!(strs.contains(&"world".to_string()));
    }

    #[test]
    fn test_va_args_comma_suppression_with_args() {
        // ,##__VA_ARGS__ with arguments - comma should remain
        let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello, world)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"hello".to_string()));
        assert!(strs.contains(&",".to_string()));
        assert!(strs.contains(&"world".to_string()));
    }

    #[test]
    fn test_va_args_comma_suppression_no_args() {
        // ,##__VA_ARGS__ without variadic arguments - comma should be suppressed
        let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"hello".to_string()));
        // The comma should be suppressed when __VA_ARGS__ is empty
        // Count commas - should be 0 or fewer than with args
        let comma_count = strs.iter().filter(|s| *s == ",").count();
        assert_eq!(
            comma_count, 0,
            "Comma should be suppressed when VA_ARGS is empty"
        );
    }

    #[test]
    fn test_va_args_comma_suppression_multiple_args() {
        // ,##__VA_ARGS__ with multiple variadic arguments
        let input = "#define DEBUG(fmt, ...) fmt, ##__VA_ARGS__\nDEBUG(hello, a, b, c)";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"hello".to_string()));
        assert!(strs.contains(&"a".to_string()));
        assert!(strs.contains(&"b".to_string()));
        assert!(strs.contains(&"c".to_string()));
    }

    #[test]
    fn test_chained_paste_expansion() {
        // Token paste creates function-like macro name, arguments come from outside.
        // This tests the case: CALL(ADD)(10, 32) where ## creates ADD_func,
        // and then (10, 32) from outside should trigger ADD_func expansion.
        let (tokens, idents) = preprocess_str(
            "#define ADD_func(x, y) ((x) + (y))\n\
             #define CONCAT(a, b) a ## b\n\
             #define CALL(name) CONCAT(name, _func)\n\
             CALL(ADD)(10, 32)",
        );
        let strs = get_token_strings(&tokens, &idents);
        // Should expand to ((10) + (32))
        assert!(strs.contains(&"10".to_string()));
        assert!(strs.contains(&"32".to_string()));
        assert!(strs.contains(&"+".to_string()));
        // Should NOT contain ADD_func as unexpanded identifier
        assert!(!strs.contains(&"ADD_func".to_string()));
    }

    // ========================================================================
    // _Pragma operator tests (C99)
    // ========================================================================

    #[test]
    fn test_pragma_operator_basic() {
        // _Pragma("...") should be silently consumed
        let (tokens, idents) = preprocess_str("_Pragma(\"GCC diagnostic ignored\") int x;");
        let strs = get_token_strings(&tokens, &idents);
        // _Pragma should be consumed, only "int x ;" should remain
        assert!(strs.contains(&"int".to_string()));
        assert!(strs.contains(&"x".to_string()));
        assert!(!strs.contains(&"_Pragma".to_string()));
    }

    #[test]
    fn test_pragma_operator_from_macro() {
        // _Pragma from macro expansion
        let (tokens, idents) = preprocess_str(
            "#define PRAGMA(x) _Pragma(#x)\n\
             #define DISABLE_WARNING(w) PRAGMA(GCC diagnostic ignored #w)\n\
             DISABLE_WARNING(-Wsign-compare)\n\
             int y;",
        );
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"int".to_string()));
        assert!(strs.contains(&"y".to_string()));
        // _Pragma should be consumed
        assert!(!strs.contains(&"_Pragma".to_string()));
    }

    #[test]
    fn test_pragma_operator_multiple() {
        // Multiple _Pragma operators
        let (tokens, idents) =
            preprocess_str("_Pragma(\"once\") _Pragma(\"GCC diagnostic push\") int z;");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"int".to_string()));
        assert!(strs.contains(&"z".to_string()));
        assert!(!strs.contains(&"_Pragma".to_string()));
    }
}
