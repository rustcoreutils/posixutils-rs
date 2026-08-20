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

use super::lexer::{
    tokens_to_text, IdentTable, LexerMode, Position, SpecialToken, Token, TokenType, TokenValue,
    Tokenizer,
};
use crate::arch;
use crate::builtin_headers;
use crate::diag;
use crate::os;
use crate::target::{Target, STDC_VERSION};
use gettextrs::gettext;

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

/// Describe how a redefinition differs from the existing definition, or `None`
/// when C17 6.10.3p2 permits it (same kind, same parameter spelling, identical
/// replacement list).
///
/// "Identical" covers white-space separation as well as spelling, but all
/// white-space separations count as identical — which is exactly what
/// `MacroToken::whitespace` records.
///
/// Whitespace *before the first* replacement token is not a separation within
/// the list, so it is ignored. That matters in practice: a predefined macro's
/// body is built from a bare value with no leading space, while the same
/// definition written as `#define __GLIBC__ 2` in a header has one — and
/// glibc's `features.h` redefines several macros we predefine, which would
/// otherwise warn on every single compilation.
fn macro_redefinition_conflict(old: &Macro, new: &Macro) -> Option<&'static str> {
    if old.builtin.is_some() {
        return Some("it is a built-in macro");
    }
    // The constraint governs redefinition "by another #define preprocessing
    // directive". A macro the implementation supplied is not one, and holding
    // headers to it is pure noise: we predefine __GLIBC_MINOR__ as 17 while
    // the host's features.h defines the true value, so every compilation
    // against glibc would warn.
    if old.predefined {
        return None;
    }
    if old.is_function != new.is_function {
        return Some("one definition is function-like and the other is not");
    }
    if old.is_function {
        if old.params.len() != new.params.len() {
            return Some("the definitions take different numbers of parameters");
        }
        if old
            .params
            .iter()
            .zip(&new.params)
            .any(|(a, b)| a.name != b.name)
        {
            return Some("the parameters are spelled differently");
        }
        if old.is_variadic != new.is_variadic || old.variadic_name != new.variadic_name {
            return Some("the definitions disagree about the variadic parameter");
        }
    }
    if !replacement_lists_identical(&old.body, &new.body) {
        return Some("the replacement lists differ");
    }
    None
}

/// Compare two replacement lists, ignoring whitespace before the first token.
fn replacement_lists_identical(a: &[MacroToken], b: &[MacroToken]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b).enumerate().all(|(i, (x, y))| {
        x.typ == y.typ && x.value == y.value && (i == 0 || x.whitespace == y.whitespace)
    })
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
    /// Is this a variadic macro (`...`)?
    pub is_variadic: bool,
    /// For the GNU named-variadic form `#define F(a, rest...)`, the name bound
    /// to the trailing arguments. `None` means the C99 spelling, where they
    /// are reached through `__VA_ARGS__`.
    pub variadic_name: Option<String>,
    /// Built-in expand function (for __LINE__, __FILE__, etc.)
    pub builtin: Option<BuiltinMacro>,
    /// True when the implementation supplied this macro (a predefine, a
    /// keyword alias, or a `-D` on the command line) rather than a `#define`
    /// directive in a translation unit.
    pub predefined: bool,
}

/// A token stored in a macro body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroToken {
    pub typ: TokenType,
    pub value: MacroTokenValue,
    pub whitespace: bool,
}

/// Value of a macro token
#[derive(Debug, Clone, PartialEq, Eq)]
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
            Some(v) => {
                // Tokenize the value string properly so (-1021) becomes (, -, 1021, )
                let mut idents = IdentTable::new();
                let mut tokenizer = Tokenizer::new(v.as_bytes(), 0, &mut idents);
                let tokens = tokenizer.tokenize();

                tokens
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
                            TokenValue::WideString(s)
                            | TokenValue::Utf16String(s)
                            | TokenValue::Utf32String(s) => MacroTokenValue::String(s.clone()),
                            TokenValue::WideChar(c)
                            | TokenValue::Utf16Char(c)
                            | TokenValue::Utf32Char(c) => MacroTokenValue::Char(c.clone()),
                            TokenValue::None => MacroTokenValue::None,
                        };
                        MacroToken {
                            typ: token.typ,
                            value,
                            whitespace: i > 0 && token.pos.whitespace,
                        }
                    })
                    .collect()
            }
            None => vec![],
        };
        Self {
            name: name.to_string(),
            body,
            is_function: false,
            params: vec![],
            is_variadic: false,
            variadic_name: None,
            builtin: None,
            predefined: true,
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
            is_variadic: false,
            variadic_name: None,
            builtin: None,
            predefined: true,
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
            is_variadic: false,
            variadic_name: None,
            builtin: None,
            predefined: true,
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
            is_variadic: false,
            variadic_name: None,
            builtin: Some(builtin),
            predefined: true,
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
    /// Apply translation phase 1 trigraph replacement to included files.
    trigraphs: bool,

    /// Whether to use system include paths (disabled by -nostdinc)
    use_system_headers: bool,

    /// Index of current file's system include path (for #include_next)
    /// None if current file is not from a system include path
    current_include_path_index: Option<usize>,

    /// Lexer mode for tokenizing included files (C or Assembly)
    lexer_mode: LexerMode,

    /// Line offset from #line directive: actual_line = token_line + line_offset
    line_offset: i32,

    /// File name override from #line directive
    line_file_override: Option<String>,

    /// The input is already the output of `c17 -E`, so translation phases 1
    /// through 4 must not run again (POSIX 87982-87983). See the allowlist in
    /// `handle_directive`.
    preprocessed: bool,

    /// Attribution established by the most recent `# N "file" flags`
    /// linemarker, if any. See [`LineMarker`].
    linemarker: Option<LineMarker>,

    /// Position of the token currently being dispatched, before [`LineMarker`]
    /// remapping. A linemarker's delta is measured from the physical line and
    /// binds to the physical stream, so both have to survive the remap that is
    /// applied to its own `#` token.
    physical_line: u32,
    physical_stream: u16,
}

/// Where a linemarker says the text after it really came from.
///
/// `c17 -E` writes `# N "file" flags` at every file transition, and POSIX
/// 87981 makes that output a `.i` operand. Compiling one has to attribute
/// diagnostics to the original file and line rather than to the position in
/// the preprocessed text, which is what GCC does and what c17 did not.
///
/// The mapping is applied by rewriting token positions rather than by
/// consulting a side table at diagnostic time: `Position` already carries a
/// stream and a line, so a rewritten token needs no further interpretation --
/// `effective_position`, the include-chain note and the `-E` marker writer all
/// keep working unchanged, and re-running `-E` over a `.i` re-emits markers
/// naming the original source rather than the `.i` it is reading.
#[derive(Debug, Clone, Copy)]
struct LineMarker {
    /// Only tokens from this stream are remapped. Text spliced in from an
    /// `#include` carries its own attribution and must not be touched.
    origin: u16,
    /// The stream the marker named.
    target: u16,
    /// Added to a physical line number to get the reported one.
    delta: i64,
}

/// What a `#pragma pack` directive does to the packing state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackAction {
    /// `#pragma pack(n)` / `#pragma pack()` -- set or clear the cap.
    Set(Option<u32>),
    /// `#pragma pack(push, n)` / `#pragma pack(push)` -- save, then set.
    Push(Option<u32>),
    /// `#pragma pack(pop)` -- restore the saved cap.
    Pop,
}

impl PackAction {
    /// Encoded into the marker token's payload, since `TokenValue` carries
    /// strings rather than arbitrary data.
    fn encode(self) -> String {
        match self {
            PackAction::Set(None) => "pack:set".to_string(),
            PackAction::Set(Some(n)) => format!("pack:set:{n}"),
            PackAction::Push(None) => "pack:push".to_string(),
            PackAction::Push(Some(n)) => format!("pack:push:{n}"),
            PackAction::Pop => "pack:pop".to_string(),
        }
    }

    /// Spell the action back as the directive that produced it.
    ///
    /// `-E` output is a `.i` operand (POSIX 87981), so what it writes for a
    /// pragma has to be C that the compiler -- ours or anyone's -- will read
    /// back. `encode` is an internal payload and was reaching the output.
    pub fn to_pragma_text(self) -> String {
        match self {
            PackAction::Set(None) => "#pragma pack()".to_string(),
            PackAction::Set(Some(n)) => format!("#pragma pack({n})"),
            PackAction::Push(None) => "#pragma pack(push)".to_string(),
            PackAction::Push(Some(n)) => format!("#pragma pack(push, {n})"),
            PackAction::Pop => "#pragma pack(pop)".to_string(),
        }
    }

    /// Recover the action from a `TokenType::Pragma` marker's payload.
    pub fn from_token(token: &Token) -> Option<PackAction> {
        match &token.value {
            TokenValue::String(s) => PackAction::decode(s),
            _ => None,
        }
    }

    fn decode(s: &str) -> Option<PackAction> {
        let mut parts = s.split(':');
        if parts.next()? != "pack" {
            return None;
        }
        let verb = parts.next()?;
        let n = match parts.next() {
            Some(v) => Some(v.parse::<u32>().ok()?),
            None => None,
        };
        match verb {
            "set" => Some(PackAction::Set(n)),
            "push" => Some(PackAction::Push(n)),
            "pop" => Some(PackAction::Pop),
            _ => None,
        }
    }
}

/// Strip pragma markers from a preprocessed token stream, reporting where
/// each one stood.
///
/// Returns `(index, action)` pairs where `index` counts tokens in the
/// *returned* stream, so the parser can apply each directive as its own
/// cursor reaches it. Doing this after preprocessing finishes is what makes
/// the ordering trustworthy: by then every include has been spliced in and
/// the vector is the translation unit in the order the parser walks it.
pub fn extract_pragma_directives(tokens: &mut Vec<Token>) -> Vec<(usize, PackAction)> {
    let mut directives = Vec::new();
    let mut kept = 0usize;
    tokens.retain(|t| {
        if t.typ != TokenType::Pragma {
            kept += 1;
            return true;
        }
        if let TokenValue::String(s) = &t.value {
            if let Some(action) = PackAction::decode(s) {
                directives.push((kept, action));
            }
        }
        false
    });
    directives
}

/// The only shapes `#pragma pack` bodies are made of.
///
/// `#pragma pack(...)` arrives as preprocessor tokens and `_Pragma("pack(...)")`
/// as a string, so both are reduced to this before being read. One reduction
/// each, and one rule -- rather than two parsers that agree until they don't.
#[derive(Debug, Clone, PartialEq, Eq)]
enum PackTok {
    Word(String),
    Num(String),
    Punct(char),
}

/// Parse the body of a `pack` pragma, `(` through `)`.
///
/// Accepts the four forms gcc and MSVC share: `pack(n)` sets the cap,
/// `pack()` clears it, `pack(push, n)` / `pack(push)` save it, and
/// `pack(pop)` restores. An unrecognised body is a pragma we do not
/// implement, and is ignored rather than guessed at.
fn parse_pack_body(toks: &[PackTok], pos: Position) -> Option<PackAction> {
    let mut it = toks.iter();
    if it.next() != Some(&PackTok::Punct('(')) {
        diag::warning(pos, &gettext("expected '(' after '#pragma pack'"));
        return None;
    }

    // C has no meaning for a non-power-of-two alignment, and gcc rejects it;
    // a bad value is dropped rather than applied.
    let number = |t: Option<&PackTok>| -> Option<u32> {
        let PackTok::Num(text) = t? else {
            diag::warning(pos, &gettext("expected an alignment in '#pragma pack'"));
            return None;
        };
        match text.parse::<u32>() {
            Ok(n) if n > 0 && n.is_power_of_two() && n <= 16 => Some(n),
            _ => {
                diag::warning(
                    pos,
                    &gettext("alignment in '#pragma pack' must be 1, 2, 4, 8 or 16"),
                );
                None
            }
        }
    };

    match it.next() {
        // `pack()` -- clear.
        Some(PackTok::Punct(')')) => Some(PackAction::Set(None)),
        Some(PackTok::Word(w)) if w == "push" => match it.next() {
            Some(PackTok::Punct(',')) => Some(PackAction::Push(number(it.next()))),
            _ => Some(PackAction::Push(None)),
        },
        Some(PackTok::Word(w)) if w == "pop" => Some(PackAction::Pop),
        Some(PackTok::Word(_)) => {
            diag::warning(pos, &gettext("unrecognized '#pragma pack' argument"));
            None
        }
        t @ Some(PackTok::Num(_)) => Some(PackAction::Set(number(t))),
        _ => {
            diag::warning(pos, &gettext("expected an alignment in '#pragma pack'"));
            None
        }
    }
}

/// Reduce the text of a `_Pragma("...")` operand to `PackTok`s.
///
/// Returns `None` for anything that is not a `pack` pragma, which is every
/// pragma c17 does not act on.
fn parse_pragma_text(body: &str, pos: Position) -> Option<PackAction> {
    let mut chars = body.trim_start().chars().peekable();
    let mut word = String::new();
    while chars
        .peek()
        .is_some_and(|c| c.is_alphanumeric() || *c == '_')
    {
        word.push(chars.next().unwrap());
    }
    if word != "pack" {
        return None;
    }

    let mut toks = Vec::new();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
        } else if c.is_ascii_digit() {
            let mut n = String::new();
            while chars.peek().is_some_and(|d| d.is_ascii_digit()) {
                n.push(chars.next().unwrap());
            }
            toks.push(PackTok::Num(n));
        } else if c.is_alphabetic() || c == '_' {
            let mut w = String::new();
            while chars
                .peek()
                .is_some_and(|d| d.is_alphanumeric() || *d == '_')
            {
                w.push(chars.next().unwrap());
            }
            toks.push(PackTok::Word(w));
        } else {
            chars.next();
            toks.push(PackTok::Punct(c));
        }
    }
    parse_pack_body(&toks, pos)
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
    /// `search` supplies the sysroot and the `-isystem` / `-idirafter`
    /// directories, which have to be known here rather than applied later:
    /// `init_predefined_macros` probes the include path to decide
    /// `__STDC_NO_THREADS__`, so the list must already be the real one.
    pub fn new(target: &'a Target, filename: &str, search: &SystemSearch<'_>) -> Self {
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
            trigraphs: false,
            use_system_headers: true,
            current_include_path_index: None,
            lexer_mode: LexerMode::C,
            line_offset: 0,
            line_file_override: None,
            preprocessed: false,
            linemarker: None,
            physical_line: 0,
            physical_stream: 0,
        };

        // Include paths first: __STDC_NO_THREADS__ is decided by probing them.
        //
        // One list, in gcc's order: `-isystem` ahead of the target's own
        // directories, `-idirafter` behind them. Keeping it as a single vector
        // is what lets `#include_next`'s index walk stay coherent.
        for path in search.isystem {
            pp.system_include_paths.push(path.clone());
        }
        for path in os::get_include_paths(target, search.sysroot) {
            pp.system_include_paths.push(path);
        }
        for path in search.idirafter {
            pp.system_include_paths.push(path.clone());
        }

        pp.init_predefined_macros();

        pp
    }

    /// Initialize predefined macros
    fn init_predefined_macros(&mut self) {
        // Standard C macros. c17 compiles one language -- C17 plus the GNU
        // extensions it has always provided -- so the version is fixed.
        // `-std=` cannot change it; see `classify_std`.
        self.define_macro(Macro::predefined("__STDC__", Some("1")));
        self.define_macro(Macro::predefined("__STDC_VERSION__", Some(STDC_VERSION)));
        self.define_macro(Macro::predefined("__STDC_HOSTED__", Some("1")));

        // __STRICT_ANSI__ is never defined: there is no strict mode to
        // advertise, and claiming one would tell system headers to hide the
        // extensions this compiler does provide.

        // C17 6.10.8.3 conditional feature macros.
        //
        // Floating point is native IEEE-754 on both targets, so
        // __STDC_IEC_559__ is warranted; without it, conforming numeric code
        // takes a needlessly conservative path. wchar_t holds Unicode code
        // points on the supported platforms, which is what
        // __STDC_ISO_10646__ asserts (the value is the Unicode revision, as
        // GCC reports it).
        self.define_macro(Macro::predefined("__STDC_IEC_559__", Some("1")));
        self.define_macro(Macro::predefined("__STDC_ISO_10646__", Some("201706L")));

        // __STDC_UTF_16__ / __STDC_UTF_32__ describe char16_t / char32_t.
        self.define_macro(Macro::predefined("__STDC_UTF_16__", Some("1")));
        self.define_macro(Macro::predefined("__STDC_UTF_32__", Some("1")));

        // __STDC_IEC_559_COMPLEX__ is deliberately NOT defined. It asserts
        // conformance to Annex G. The original reason -- that complex support
        // was broken outright -- no longer holds: #C1/#C2 are fixed, and the
        // arithmetic is now byte-identical to gcc's at float, double and long
        // double. What is still missing is G.5.1p4: an infinite operand must
        // give an infinite result even against a NaN, and
        // `CMPLX(INFINITY,0) * CMPLX(NAN,NAN)` yields NaN here.
        //
        // gcc fails that same rule and defines the macro regardless, so this
        // is a deliberate divergence: the macro is a claim about the
        // arithmetic, and the arithmetic does not support it yet. Pinned by
        // `c17_complex_infinity_rules_match_gcc`, which fails the day G.5.1
        // is implemented -- the signal to revisit this rather than inherit it.

        // C17 4p6: an implementation that does not provide <threads.h> shall
        // define __STDC_NO_THREADS__, so portable code can feature-test rather
        // than fail at the include. We bundle no threads.h and rely on the
        // host's, so this is purely a question about the host.
        if !self.host_has_threads_header() {
            self.define_macro(Macro::predefined("__STDC_NO_THREADS__", Some("1")));
        }

        // GCC compatibility macros (required by system headers).
        //
        // The claimed version is a statement about which header paths this
        // compiler can take, and it is measured rather than aspirational.
        // 6.5.0 is the highest that works against glibc 2.39:
        //
        //   4.3  `bits/floatn.h` turns on __HAVE_FLOAT128 and needs the
        //        `__float128` keyword and `_Complex float` with mode(TC)
        //   4.4  `<math.h>` switches isnan/isinf/isfinite/isnormal/fpclassify
        //        onto the builtins -- the point of claiming any of this, since
        //        the `sizeof` ternary it replaces calls `__isnanl`, which
        //        answers 65535 rather than 1
        //   4.9  __HAVE_GENERIC_SELECTION turns on, so __MATH_TG uses
        //        `_Generic` rather than __builtin_choose_expr
        //   6.0  `signbit` goes to __builtin_signbit*
        //   7.0  __HAVE_FLOATN_NOT_TYPEDEF makes _FloatN native types, and
        //        `stdlib.h` then declares `strtof32x` in terms of a `_Float32x`
        //        this compiler does not have. That is the ceiling.
        self.define_macro(Macro::predefined("__GNUC__", Some("6")));
        self.define_macro(Macro::predefined("__GNUC_MINOR__", Some("5")));
        self.define_macro(Macro::predefined("__GNUC_PATCHLEVEL__", Some("0")));
        self.define_macro(Macro::predefined(
            "__VERSION__",
            Some(concat!(
                "\"c17 ",
                env!("CARGO_PKG_VERSION"),
                " (gcc compatible 6.5.0)\""
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

        // Note: C99/C11 bool, true, false are NOT pre-defined here.
        // They should only be available after #include <stdbool.h>.
        // This matches GCC/Clang behavior. _Bool is the builtin type.

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

        // OS macros, including the unreserved `unix` / `linux` spellings.
        // Predefining names outside the implementation's reserved namespace is
        // a GNU extension, and this compiler is always in that mode.
        for (name, value) in os::get_os_macros(self.target) {
            self.define_macro(Macro::predefined(name, value.as_deref()));
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

    /// Whether the host provides `<threads.h>` on any system include path.
    ///
    /// Probed rather than assumed: glibc gained it in 2.28, musl has it, and
    /// macOS still does not, so the answer varies by host even for one target.
    fn host_has_threads_header(&self) -> bool {
        self.system_include_paths
            .iter()
            .any(|dir| Path::new(dir).join("threads.h").exists())
    }

    /// Define a macro
    pub fn define_macro(&mut self, mac: Macro) {
        self.macros.insert(mac.name.clone(), mac);
    }

    /// Apply one command-line `-D` specification.
    ///
    /// The spec is rewritten as the equivalent `#define` directive and run
    /// through the ordinary directive path, so `-D'F(x)=x+1'` gets the same
    /// parameter parsing, `#`/`##` handling and variadic support as a `#define`
    /// in source. Building an object-like macro directly made `"F(x)"` the
    /// macro *name* — a silent no-op, since no such identifier can ever be
    /// written in a translation unit.
    pub fn define_from_cmdline(&mut self, spec: &str, idents: &mut IdentTable) {
        // `-DNAME` with no `=` defines NAME as 1.
        let text = match spec.find('=') {
            Some(eq) => format!("{} {}\n", &spec[..eq], &spec[eq + 1..]),
            None => format!("{} 1\n", spec),
        };

        let stream_id = diag::init_stream("<command-line>");
        let tokens = {
            let mut tokenizer = Tokenizer::new(text.as_bytes(), stream_id, idents);
            tokenizer.tokenize()
        };
        // handle_define expects to start at the macro name, and stream markers
        // would be taken for one.
        let mut iter = tokens
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .peekable();
        self.handle_define(&mut iter, idents);
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

        while let Some(mut token) = iter.next() {
            // Attribute the token before anything looks at it, so that macro
            // expansion, diagnostics and the `-E` marker writer all inherit
            // the position a linemarker established. The physical position is
            // kept because the next linemarker's delta is measured from it.
            self.physical_line = token.pos.line;
            self.physical_stream = token.pos.stream;
            token.pos = self.remap_pos(token.pos);

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
                    // In an already-preprocessed file every macro has already
                    // been expanded, so nothing here is a macro name: not a
                    // `#define` recorded above (GCC records them for debug
                    // info but never substitutes), not a `-D` from the command
                    // line, and not a predefined `__LINE__` or `__STDC__`.
                    // `_Pragma` is a phase-4 operator and is likewise spent --
                    // `c17 -E` has already lowered it to a `#pragma` line.
                    if self.preprocessed {
                        output.push(token);
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
                                self.handle_pragma_operator(&mut iter, &mut output);
                                continue;
                            }

                            // Check blue-painting: if this token was marked as no-expand
                            // for this macro (from a previous expansion), don't expand it.
                            // Per C99 6.10.3.4, macro names in their own expansion are
                            // "blue painted" and should not be re-expanded.
                            if token.is_no_expand(&name) {
                                output.push(token);
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

                                    // Check blue-painting on the token itself
                                    if last_token.is_no_expand(&macro_name) {
                                        output.push(last_token);
                                        break;
                                    }
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
        // C17 6.10p7: a `#` alone on a line is the null directive and has no
        // effect. Check before consuming, because the next token belongs to the
        // *next* line — taking it unconditionally treated that line's first
        // token as a directive name and then `skip_to_eol` ate the rest of it,
        // silently deleting a line of source.
        match iter.peek() {
            None => return,
            Some(next) if next.pos.newline => return,
            Some(_) => {}
        }

        // Get the directive name
        let directive_token = match iter.next() {
            Some(t) => t,
            None => return, // Empty directive, ignore
        };

        // Get directive StringId
        let directive_id = match &directive_token.typ {
            TokenType::Ident => {
                if let TokenValue::Ident(id) = &directive_token.value {
                    Some(*id)
                } else {
                    None
                }
            }
            _ => None,
        };

        let directive_id = match directive_id {
            Some(id) => id,
            None => {
                // A number where a directive name belongs is the GCC
                // linemarker `# N "file" flags` -- the form `c17 -E` writes
                // and POSIX 87981 makes a `.i` operand. It used to be
                // swallowed whole, which is why every diagnostic about a
                // preprocessed file cited the position in the preprocessed
                // text rather than the original.
                if directive_token.typ == TokenType::Number && !self.is_skipping() {
                    self.handle_linemarker(iter, &directive_token);
                    return;
                }
                // Consume rest of line
                self.skip_to_eol(iter);
                return;
            }
        };

        // POSIX 87982-87983: the processing `c17 -E` already performed shall
        // not be repeated. That is not a wholesale skip of translation phase
        // 4 -- GCC keeps a five-directive allowlist (libcpp's `IN_I` set), and
        // `#pragma pack` in particular has to keep working or a preprocessed
        // file silently loses its layout. Everything outside the allowlist
        // leaves the `#` as a stray token, which is what GCC diagnoses.
        if self.preprocessed
            && !matches!(
                directive_id,
                crate::kw::DEFINE
                    | crate::kw::UNDEF
                    | crate::kw::PRAGMA
                    | crate::kw::PP_IDENT
                    | crate::kw::SCCS
            )
            && matches!(
                directive_id,
                crate::kw::IFDEF
                    | crate::kw::IFNDEF
                    | crate::kw::IF
                    | crate::kw::ELIF
                    | crate::kw::ELSE
                    | crate::kw::ENDIF
                    | crate::kw::INCLUDE
                    | crate::kw::INCLUDE_NEXT
                    | crate::kw::PP_ERROR
                    | crate::kw::WARNING
                    | crate::kw::LINE
            )
        {
            // One diagnostic naming the cause, rather than GCC's cascade of
            // follow-on parse errors from feeding the rest of the line to the
            // parser.
            diag::error(hash_token.pos, "stray '#' in program");
            self.skip_to_eol(iter);
            return;
        }

        match directive_id {
            crate::kw::DEFINE => self.handle_define(iter, idents),
            crate::kw::UNDEF => self.handle_undef(iter, idents),
            crate::kw::IFDEF => self.handle_ifdef(iter, idents, hash_token.pos),
            crate::kw::IFNDEF => self.handle_ifndef(iter, idents, hash_token.pos),
            crate::kw::IF => self.handle_if(iter, idents, hash_token.pos),
            crate::kw::ELIF => self.handle_elif(iter, idents),
            crate::kw::ELSE => self.handle_else(iter),
            crate::kw::ENDIF => self.handle_endif(iter),
            crate::kw::INCLUDE => self.handle_include(iter, output, idents, hash_token, false),
            crate::kw::INCLUDE_NEXT => self.handle_include(iter, output, idents, hash_token, true),
            crate::kw::PP_ERROR => self.handle_error(iter, &hash_token.pos, idents),
            crate::kw::WARNING => self.handle_warning(iter, &hash_token.pos, idents),
            crate::kw::PRAGMA => self.handle_pragma(iter, output, idents),
            crate::kw::LINE => self.handle_line(iter, idents, hash_token.pos),
            _ => {
                // Unknown directive
                if !self.is_skipping() {
                    let name = idents.get_opt(directive_id).unwrap_or("unknown");
                    diag::warning_args(
                        hash_token.pos,
                        "unknown preprocessor directive #{0}",
                        &[name],
                    );
                }
                self.skip_to_eol(iter);
            }
        }
    }

    /// Apply the active [`LineMarker`] to a position.
    fn remap_pos(&self, pos: Position) -> Position {
        match self.linemarker {
            Some(lm) if lm.origin == pos.stream => Position {
                stream: lm.target,
                line: (pos.line as i64 + lm.delta).max(1) as u32,
                ..pos
            },
            _ => pos,
        }
    }

    /// Consume a `# N ["file" [flags]]` linemarker and record the attribution
    /// it establishes for the text that follows.
    ///
    /// `number` is the directive-name token, which the caller has already
    /// taken from the iterator and found to be a number rather than an
    /// identifier. C17 has no such directive; this is the GCC form that
    /// `c17 -E` itself writes, and `#line` is deliberately *not* routed here
    /// (GCC honors only this form in a preprocessed file, and so does c17).
    fn handle_linemarker<I>(&mut self, iter: &mut std::iter::Peekable<I>, number: &Token)
    where
        I: Iterator<Item = Token>,
    {
        let origin = self.physical_stream;
        let TokenValue::Number(ref text) = number.value else {
            self.skip_to_eol(iter);
            return;
        };
        let Ok(line) = text.parse::<u32>() else {
            self.skip_to_eol(iter);
            return;
        };

        // An optional filename follows; without one the marker renumbers the
        // current file rather than renaming it.
        let mut target = self.linemarker.map_or(origin, |lm| lm.target);
        if let Some(tok) = iter.peek() {
            if !tok.pos.newline && tok.typ == TokenType::String {
                if let TokenValue::String(name) = &tok.value {
                    let name = name.clone();
                    target = diag::find_or_add_stream(&name);
                }
                iter.next();
            }
        }

        // Flags 1 (entering) and 2 (returning) are attribution the stream
        // registry already derives; 4 (extern "C") means nothing here. Only 3
        // carries a decision: it marks a system header, whose warnings are
        // not the user's to act on.
        let mut is_system = false;
        while let Some(tok) = iter.peek() {
            if tok.pos.newline {
                break;
            }
            if let TokenValue::Number(flag) = &tok.value {
                if flag == "3" {
                    is_system = true;
                }
            }
            iter.next();
        }
        diag::set_stream_system(target, is_system);

        // The marker names the line of the text *after* it, so the delta is
        // measured against the next physical line.
        self.linemarker = Some(LineMarker {
            origin,
            target,
            delta: line as i64 - (self.physical_line as i64 + 1),
        });
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
                            no_expand: None,
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
                            no_expand: None,
                        });
                    }
                } else {
                    // Unknown identifier -> 0
                    final_result.push(Token {
                        typ: TokenType::Number,
                        value: TokenValue::Number("0".to_string()),
                        pos: token.pos,
                        no_expand: None,
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
        let mut params: Vec<MacroParam> = Vec::new();
        let mut is_function = false;
        let mut is_variadic = false;
        let mut variadic_name = None;

        if let Some(next) = iter.peek() {
            if !next.pos.whitespace {
                if let TokenValue::Special(code) = &next.value {
                    if *code == b'(' as u32 {
                        is_function = true;
                        iter.next(); // consume '('

                        // Parse parameters
                        let mut param_index = 0;
                        // Whether the token just parsed was an identifier with
                        // no comma after it yet. `...` directly behind such an
                        // identifier is the GNU named-variadic spelling
                        // `#define F(a, rest...)`, where `rest` names the
                        // trailing arguments rather than being a parameter of
                        // its own.
                        let mut ident_immediately_before = false;
                        while let Some(param_tok) = iter.next() {
                            if param_tok.pos.newline {
                                break;
                            }

                            match &param_tok.value {
                                TokenValue::Special(c) if *c == b')' as u32 => break,
                                TokenValue::Special(c) if *c == b',' as u32 => {
                                    ident_immediately_before = false;
                                    continue;
                                }
                                TokenValue::Special(c) if *c == SpecialToken::Ellipsis as u32 => {
                                    is_variadic = true;
                                    if ident_immediately_before {
                                        // Rebind the identifier: it is the name
                                        // of the variadic part, not a positional
                                        // parameter. Leaving it in `params` made
                                        // it match only the *first* trailing
                                        // argument and pushed the __VA_ARGS__
                                        // start index one too far.
                                        if let Some(p) = params.pop() {
                                            variadic_name = Some(p.name);
                                        }
                                    }
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
                                        ident_immediately_before = true;
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
        let body = self.tokens_to_macro_body(
            &body_tokens,
            &params,
            variadic_name.as_deref(),
            is_function,
            idents,
        );

        let mac = Macro {
            name: name.clone(),
            body,
            is_function,
            params,
            is_variadic,
            variadic_name,
            builtin: None,
            predefined: false,
        };

        // C17 6.10.3p2: a macro may be redefined only by a definition of the
        // same kind, with the same parameter spelling and an identical
        // replacement list. Diagnose as a warning rather than an error: the
        // standard requires only a diagnostic, and rejecting outright would
        // break a great deal of code that redefines a macro benignly.
        if let Some(existing) = self.macros.get(&name) {
            if let Some(why) = macro_redefinition_conflict(existing, &mac) {
                diag::warning_args(
                    name_token.pos,
                    "'{0}' redefined: {1}",
                    &[&name.to_string(), why],
                );
            }
        }

        self.define_macro(mac);
    }

    /// Convert tokens to macro body
    fn tokens_to_macro_body(
        &self,
        tokens: &[Token],
        params: &[MacroParam],
        variadic_name: Option<&str>,
        is_function: bool,
        idents: &IdentTable,
    ) -> Vec<MacroToken> {
        // C17 6.10.3.3p1: `##` shall occur at neither end of a replacement
        // list. Both used to become a literal `#`/`##` token instead.
        let is_paste = |t: &Token| {
            matches!(&t.value, TokenValue::Special(c)
                if *c == SpecialToken::HashHash as u32)
        };
        if let Some(first) = tokens.first() {
            if is_paste(first) {
                diag::error(
                    first.pos,
                    &gettext("'##' cannot appear at the start of a macro replacement list"),
                );
            }
        }
        if tokens.len() > 1 {
            if let Some(last) = tokens.last() {
                if is_paste(last) {
                    diag::error(
                        last.pos,
                        &gettext("'##' cannot appear at the end of a macro replacement list"),
                    );
                }
            }
        }

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
                                // Check for __VA_ARGS__, or the GNU named
                                // variadic that stands in for it.
                                if name == "__VA_ARGS__" || Some(name) == variadic_name {
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

                    // C17 6.10.3.2p1: in a function-like macro each `#` shall
                    // be followed by a parameter. It used to fall through to a
                    // literal `#` token with no diagnostic.
                    if is_function {
                        diag::error(
                            token.pos,
                            &gettext("'#' is not followed by a macro parameter"),
                        );
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
                    // Check if it's __VA_ARGS__, or the GNU named variadic
                    // that stands in for it.
                    if name == "__VA_ARGS__" || Some(name) == variadic_name {
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
            TokenValue::WideString(s) | TokenValue::Utf16String(s) | TokenValue::Utf32String(s) => {
                MacroTokenValue::String(s.clone())
            }
            TokenValue::WideChar(c) | TokenValue::Utf16Char(c) | TokenValue::Utf32Char(c) => {
                MacroTokenValue::Char(c.clone())
            }
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
        evaluator.evaluate(tokens).is_true()
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
            diag::error(hash_token.pos, &gettext("expected filename after #include"));
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
            diag::error(hash_token.pos, &gettext("empty filename in #include"));
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
            diag::error_args(
                hash_token.pos,
                "'{0}': file not found",
                &[&filename.to_string()],
            );
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
        // Absolute path
        if filename.starts_with('/') {
            let path = PathBuf::from(filename);
            if path.exists() {
                return Some((IncludeSource::File(path), None));
            }
            return None;
        }

        // The `"..."` form searches the including file's own directory first
        // (c17.md 87905-87910), then proceeds as for the `<...>` form.
        if !is_system && !is_include_next {
            let relative_path = Path::new(&self.current_dir).join(filename);
            if relative_path.exists() {
                return Some((IncludeSource::File(relative_path), None));
            }
        }

        // Then -I, for both forms.
        if !is_include_next {
            for dir in &self.quote_include_paths {
                let path = Path::new(dir).join(filename);
                if path.exists() {
                    return Some((IncludeSource::File(path), None));
                }
            }
        }

        // Bundled headers stand in for the compiler's own include directory,
        // so they come after the user's search paths — a project that ships
        // its own limits.h or stddef.h must win. They used to be consulted
        // before any filesystem search, which silently shadowed those.
        // #include_next skips them entirely.
        if !is_include_next && self.use_builtin_headers {
            if let Some(content) = builtin_headers::get_builtin_header(filename) {
                return Some((IncludeSource::Builtin(content), None));
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

    /// Detect include guard macro from file content.
    /// Returns Some(macro_name) if the file starts with `#ifndef MACRO` followed by `#define MACRO`.
    /// This ensures we only detect true include guards, not conditional includes like:
    ///   #ifndef _CDEFS_H_
    ///   # error "Use <sys/cdefs.h> instead"
    ///   #endif
    fn detect_include_guard(content: &[u8]) -> Option<String> {
        // Convert to string for simple parsing
        let text = match std::str::from_utf8(content) {
            Ok(s) => s,
            Err(_) => return None,
        };

        // Skip leading whitespace and find the first preprocessor directive
        let mut chars = text.chars().peekable();

        // Helper to skip whitespace and comments
        let skip_ws_and_comments = |chars: &mut std::iter::Peekable<std::str::Chars>| {
            loop {
                // Skip whitespace
                while chars.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
                    chars.next();
                }

                // Check for comment
                if chars.peek() == Some(&'/') {
                    chars.next();
                    match chars.peek() {
                        Some(&'/') => {
                            // Line comment - skip to end of line
                            while chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                                chars.next();
                            }
                            continue;
                        }
                        Some(&'*') => {
                            // Block comment - skip to */
                            chars.next();
                            while let Some(c) = chars.next() {
                                if c == '*' && chars.peek() == Some(&'/') {
                                    chars.next();
                                    break;
                                }
                            }
                            continue;
                        }
                        _ => return false, // Unexpected /
                    }
                }
                return true;
            }
        };

        if !skip_ws_and_comments(&mut chars) {
            return None;
        }

        // Now we should be at #
        if chars.next() != Some('#') {
            return None;
        }

        // Skip whitespace after #
        while chars
            .peek()
            .map(|c| *c == ' ' || *c == '\t')
            .unwrap_or(false)
        {
            chars.next();
        }

        // Collect directive name
        let mut directive = String::new();
        while chars
            .peek()
            .map(|c| c.is_ascii_alphabetic() || *c == '_')
            .unwrap_or(false)
        {
            directive.push(chars.next().unwrap());
        }

        match directive.as_str() {
            "ifndef" => {
                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }
                // Collect macro name
                let mut macro_name = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .unwrap_or(false)
                {
                    macro_name.push(chars.next().unwrap());
                }
                if macro_name.is_empty() {
                    return None;
                }

                // Skip to end of line
                while chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                    chars.next();
                }
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }

                // Skip whitespace and comments before next directive
                if !skip_ws_and_comments(&mut chars) {
                    return None;
                }

                // Now check for #define MACRO (the second part of include guard pattern)
                if chars.next() != Some('#') {
                    return None;
                }

                // Skip whitespace after #
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }

                // Collect directive name
                let mut next_directive = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphabetic() || *c == '_')
                    .unwrap_or(false)
                {
                    next_directive.push(chars.next().unwrap());
                }

                if next_directive != "define" {
                    return None;
                }

                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }

                // Collect the defined macro name
                let mut define_name = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .unwrap_or(false)
                {
                    define_name.push(chars.next().unwrap());
                }

                // The #define must define the same macro as the #ifndef
                if define_name != macro_name {
                    return None;
                }

                // Check for conditional default definition pattern:
                //   #ifndef MACRO
                //   #define MACRO [value]
                //   #endif
                //   ... more content ...  <-- content AFTER #endif = NOT a guard
                //
                // vs true include guard:
                //   #ifndef MACRO
                //   #define MACRO
                //   ... guarded content ...
                //   #endif
                //   [end of file or just whitespace/comments]

                // Skip to end of #define line
                while chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                    chars.next();
                }
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }

                // Skip whitespace and comments
                if !skip_ws_and_comments(&mut chars) {
                    return None;
                }

                // Check if next thing is #endif followed by more content
                if chars.peek() == Some(&'#') {
                    let mut check_chars = chars.clone();
                    check_chars.next(); // consume #
                                        // Skip whitespace after #
                    while check_chars
                        .peek()
                        .map(|c| *c == ' ' || *c == '\t')
                        .unwrap_or(false)
                    {
                        check_chars.next();
                    }
                    // Collect directive name
                    let mut next_dir = String::new();
                    while check_chars
                        .peek()
                        .map(|c| c.is_ascii_alphabetic() || *c == '_')
                        .unwrap_or(false)
                    {
                        next_dir.push(check_chars.next().unwrap());
                    }
                    if next_dir == "endif" {
                        // Found #endif immediately after #define
                        // Now check if there's more content after the #endif
                        // Skip to end of #endif line
                        while check_chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                            check_chars.next();
                        }
                        if check_chars.peek() == Some(&'\n') {
                            check_chars.next();
                        }
                        // Skip whitespace and comments after #endif
                        skip_ws_and_comments(&mut check_chars);
                        // If there's more content after #endif, this is NOT a guard
                        if check_chars.peek().is_some() {
                            return None;
                        }
                        // Otherwise it's a valid (empty) include guard
                    }
                }

                // Looks like a valid include guard
                return Some(macro_name);
            }
            "if" => {
                // Check for !defined(MACRO) pattern
                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }
                // Check for !
                if chars.next() != Some('!') {
                    return None;
                }
                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }
                // Collect "defined"
                let mut keyword = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphabetic())
                    .unwrap_or(false)
                {
                    keyword.push(chars.next().unwrap());
                }
                if keyword != "defined" {
                    return None;
                }
                // Skip whitespace and optional (
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }
                let has_paren = chars.peek() == Some(&'(');
                if has_paren {
                    chars.next();
                }
                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }
                // Collect macro name
                let mut macro_name = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .unwrap_or(false)
                {
                    macro_name.push(chars.next().unwrap());
                }
                if macro_name.is_empty() {
                    return None;
                }
                // Skip optional closing paren
                if has_paren {
                    while chars
                        .peek()
                        .map(|c| *c == ' ' || *c == '\t')
                        .unwrap_or(false)
                    {
                        chars.next();
                    }
                    if chars.peek() == Some(&')') {
                        chars.next();
                    }
                }

                // Skip to end of line
                while chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                    chars.next();
                }
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }

                // Skip whitespace and comments before next directive
                if !skip_ws_and_comments(&mut chars) {
                    return None;
                }

                // Now check for #define MACRO (second part of include guard pattern)
                if chars.next() != Some('#') {
                    return None;
                }

                // Skip whitespace after #
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }

                // Collect directive name
                let mut next_directive = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphabetic() || *c == '_')
                    .unwrap_or(false)
                {
                    next_directive.push(chars.next().unwrap());
                }

                if next_directive != "define" {
                    return None;
                }

                // Skip whitespace
                while chars
                    .peek()
                    .map(|c| *c == ' ' || *c == '\t')
                    .unwrap_or(false)
                {
                    chars.next();
                }

                // Collect the defined macro name
                let mut define_name = String::new();
                while chars
                    .peek()
                    .map(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .unwrap_or(false)
                {
                    define_name.push(chars.next().unwrap());
                }

                // The #define must define the same macro as the #if !defined
                if define_name != macro_name {
                    return None;
                }

                // Same check as for #ifndef case:
                // Check for conditional default definition pattern

                // Skip to end of #define line
                while chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                    chars.next();
                }
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }

                // Skip whitespace and comments
                if !skip_ws_and_comments(&mut chars) {
                    return None;
                }

                // Check if next thing is #endif followed by more content
                if chars.peek() == Some(&'#') {
                    let mut check_chars = chars.clone();
                    check_chars.next(); // consume #
                                        // Skip whitespace after #
                    while check_chars
                        .peek()
                        .map(|c| *c == ' ' || *c == '\t')
                        .unwrap_or(false)
                    {
                        check_chars.next();
                    }
                    // Collect directive name
                    let mut next_dir = String::new();
                    while check_chars
                        .peek()
                        .map(|c| c.is_ascii_alphabetic() || *c == '_')
                        .unwrap_or(false)
                    {
                        next_dir.push(check_chars.next().unwrap());
                    }
                    if next_dir == "endif" {
                        // Found #endif immediately after #define
                        // Now check if there's more content after the #endif
                        // Skip to end of #endif line
                        while check_chars.peek().map(|c| *c != '\n').unwrap_or(false) {
                            check_chars.next();
                        }
                        if check_chars.peek() == Some(&'\n') {
                            check_chars.next();
                        }
                        // Skip whitespace and comments after #endif
                        skip_ws_and_comments(&mut check_chars);
                        // If there's more content after #endif, this is NOT a guard
                        if check_chars.peek().is_some() {
                            return None;
                        }
                        // Otherwise it's a valid (empty) include guard
                    }
                }

                // Looks like a valid include guard
                return Some(macro_name);
            }
            _ => {}
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

        // Read the file first so we can check for include guards
        let content = match fs::read(path) {
            Ok(c) => c,
            Err(e) => {
                diag::error_args(
                    hash_token.pos,
                    "cannot read '{0}': {1}",
                    &[&path.display().to_string(), &e.to_string()],
                );
                return;
            }
        };

        // Translation phase 1 applies to an included file just as it does to
        // the primary source, and before the include-guard scan looks at it.
        let content = if self.trigraphs {
            crate::token::lexer::replace_trigraphs(&content).into_owned()
        } else {
            content
        };

        // Check for include guard optimization: if file starts with #ifndef MACRO
        // or #if !defined(MACRO) and that macro is already defined, skip the include.
        // This allows circular includes protected by guards to work correctly.
        if let Some(guard_macro) = Self::detect_include_guard(&content) {
            if self.is_defined(&guard_macro) {
                return;
            }
        }

        // Check for include cycle (only error if no include guard protects it)
        if self.include_stack.contains(&canonical) {
            diag::error_args(
                hash_token.pos,
                "recursive include of '{0}'",
                &[&path.display().to_string()],
            );
            return;
        }

        // Check include depth
        if self.include_depth >= self.max_include_depth {
            diag::error_args(
                hash_token.pos,
                "#include nested too deeply (max {0})",
                &[&self.max_include_depth.to_string()],
            );
            return;
        }

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

        // Create a new stream for this file, remembering which `#include`
        // brought it in: that is what lets a diagnostic inside a header name
        // the chain that reached it.
        let stream_id = diag::init_included_stream(&self.current_file, hash_token.pos);

        // Tokenize the included file using the same shared string table
        // Since we use the same StringTable, all StringIds are consistent
        // and no ID remapping is needed.
        // Use the same lexer mode as the main file (C or Assembly).
        let tokens = {
            let mut tokenizer =
                Tokenizer::new_with_mode(&content, stream_id, idents, self.lexer_mode);
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
            diag::error_args(
                hash_token.pos,
                "#include nested too deeply (max {0})",
                &[&self.max_include_depth.to_string()],
            );
            return;
        }

        // Save current state
        let saved_file = std::mem::replace(&mut self.current_file, format!("<builtin:{}>", name));
        let saved_dir = std::mem::replace(&mut self.current_dir, ".".to_string());
        let saved_cond_stack = std::mem::take(&mut self.cond_stack);

        self.include_depth += 1;

        // Create a stream for this builtin header, with the `#include` that
        // asked for it; see `include_file`.
        let stream_id = diag::init_included_stream(&self.current_file, hash_token.pos);

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
        diag::error_args(*pos, "#error {0}", &[&msg.to_string()]);
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
        diag::warning_args(*pos, "#warning {0}", &[&msg.to_string()]);
    }

    /// Reduce `#pragma pack(...)`'s tokens to `PackTok`s and read them.
    fn parse_pack_pragma<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        idents: &IdentTable,
        pos: Position,
    ) -> Option<PackAction>
    where
        I: Iterator<Item = Token>,
    {
        let mut toks = Vec::new();
        while let Some(tok) = iter.peek() {
            if tok.pos.newline && !toks.is_empty() {
                break;
            }
            let reduced = match &tok.value {
                TokenValue::Ident(id) => PackTok::Word(idents.get_opt(*id)?.to_string()),
                TokenValue::Number(n) => PackTok::Num(n.clone()),
                TokenValue::Special(c) => PackTok::Punct(char::from_u32(*c)?),
                _ => break,
            };
            let closing = reduced == PackTok::Punct(')');
            toks.push(reduced);
            iter.next();
            if closing {
                break;
            }
        }
        parse_pack_body(&toks, pos)
    }

    /// Handle #pragma
    fn handle_pragma<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        output: &mut Vec<Token>,
        idents: &IdentTable,
    ) where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Check for #pragma once and #pragma STDC
        if let Some(token) = iter.peek() {
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    if name == "pack" {
                        let pos = token.pos;
                        iter.next(); // consume "pack"
                        if let Some(action) = self.parse_pack_pragma(iter, idents, pos) {
                            // The parser decides layout, so the pragma has to
                            // reach it. It travels as a marker in the stream
                            // because that is the only ordering that survives
                            // include splicing.
                            // A directive handler pulls its own tokens, so
                            // they never pass the remap in the main loop; the
                            // marker has to be attributed here or a `.i`
                            // reports the pragma against the preprocessed file.
                            let mut marker = Token::new(TokenType::Pragma, self.remap_pos(pos));
                            marker.value = TokenValue::String(action.encode());
                            output.push(marker);
                        }
                        self.skip_to_eol(iter);
                        return;
                    } else if name == "once" {
                        if let Ok(canonical) = Path::new(&self.current_file).canonicalize() {
                            self.once_files.insert(canonical);
                        }
                    } else if name == "STDC" {
                        let pos = token.pos;
                        iter.next(); // consume "STDC"

                        // Expect pragma name: FP_CONTRACT, FENV_ACCESS, or CX_LIMITED_RANGE
                        let valid_pragma = if let Some(tok) = iter.peek() {
                            if let TokenValue::Ident(id2) = &tok.value {
                                if let Some(pname) = idents.get_opt(*id2) {
                                    matches!(
                                        pname,
                                        "FP_CONTRACT" | "FENV_ACCESS" | "CX_LIMITED_RANGE"
                                    )
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        };

                        if valid_pragma {
                            iter.next(); // consume pragma name

                            // Expect ON, OFF, or DEFAULT
                            let valid_arg = if let Some(tok) = iter.peek() {
                                if let TokenValue::Ident(id3) = &tok.value {
                                    if let Some(aname) = idents.get_opt(*id3) {
                                        matches!(aname, "ON" | "OFF" | "DEFAULT")
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                }
                            } else {
                                false
                            };

                            if valid_arg {
                                iter.next(); // consume ON/OFF/DEFAULT
                            } else {
                                diag::warning(
                                    pos,
                                    &gettext("expected ON, OFF, or DEFAULT for #pragma STDC"),
                                );
                            }
                        } else {
                            diag::warning(
                                pos,
                                &gettext("expected FP_CONTRACT, FENV_ACCESS, or CX_LIMITED_RANGE after #pragma STDC"),
                            );
                        }

                        self.skip_to_eol(iter);
                        return;
                    }
                }
            }
        }

        self.skip_to_eol(iter);
    }

    /// Handle _Pragma operator (C99)
    /// _Pragma("string") is equivalent to #pragma string
    /// Since we ignore most pragmas anyway, this just consumes the tokens
    fn handle_pragma_operator<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        output: &mut Vec<Token>,
    ) where
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

        // Expect a string literal.
        //
        // C99 6.10.9p1: destringify and re-tokenize as a `#pragma`. Only the
        // pragmas c17 acts on need that treatment; the rest stay no-ops. It
        // matters for `pack`, which changes layout -- and a `_Pragma` that
        // was quietly dropped while the `#pragma` spelling was honoured would
        // be the same wrong struct in the spelling nobody tested.
        let Some(token) = iter.next() else {
            return;
        };
        if !matches!(token.typ, TokenType::String) {
            // Not a valid _Pragma - just silently ignore
            return;
        }
        if let TokenValue::String(body) = &token.value {
            if let Some(action) = parse_pragma_text(body, token.pos) {
                let mut marker = Token::new(TokenType::Pragma, self.remap_pos(token.pos));
                marker.value = TokenValue::String(action.encode());
                output.push(marker);
            }
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
    fn handle_line<I>(
        &mut self,
        iter: &mut std::iter::Peekable<I>,
        idents: &mut IdentTable,
        directive_pos: Position,
    ) where
        I: Iterator<Item = Token>,
    {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        let tokens = self.collect_to_eol(iter);
        let tokens = self.expand_if_tokens(&tokens, idents);
        if tokens.is_empty() {
            diag::error(directive_pos, &gettext("#line requires a line number"));
            return;
        }

        // C17 6.10.4p3: the operand is a digit sequence in [1, 2147483647].
        // These used to return silently, so a typo just did nothing.
        let line_num = match &tokens[0].value {
            TokenValue::Number(n) => match n.parse::<u32>() {
                Ok(num) if (1..=2147483647).contains(&num) => num,
                Ok(_) => {
                    diag::error_args(
                        tokens[0].pos,
                        "#line number '{0}' is out of range [1, 2147483647]",
                        &[&n.to_string()],
                    );
                    return;
                }
                Err(_) => {
                    diag::error_args(
                        tokens[0].pos,
                        "#line requires a decimal line number, found '{0}'",
                        &[&n.to_string()],
                    );
                    return;
                }
            },
            _ => {
                diag::error(
                    tokens[0].pos,
                    &gettext("#line requires a decimal line number"),
                );
                return;
            }
        };

        // Only a string literal may follow, and nothing may follow that.
        if tokens.len() > 1 && !matches!(&tokens[1].value, TokenValue::String(_)) {
            diag::error(
                tokens[1].pos,
                &gettext("#line filename must be a string literal"),
            );
            return;
        }
        if tokens.len() > 2 {
            diag::error(
                tokens[2].pos,
                &gettext("extra tokens after #line directive"),
            );
            return;
        }

        // The #line directive takes effect on the next line, so
        // current_physical_line is the line of the directive + 1
        let current_physical_next_line = tokens[0].pos.line + 1;
        self.line_offset = line_num as i32 - current_physical_next_line as i32;

        // Optional second token: filename string
        if tokens.len() > 1 {
            if let TokenValue::String(s) = &tokens[1].value {
                self.line_file_override = Some(s.clone());
            }
        }
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
                TokenValue::Special(code) if *code < SpecialToken::BASE => {
                    result.push(*code as u8 as char);
                }
                // A punctuator of more than one character has a spelling too.
                // Dropping it turned `a >> b` into `a  b`, in `#x` (6.10.3.2p2
                // asks for "the spelling of the preprocessing token") and in
                // the text a `_Pragma` is rebuilt from alike.
                TokenValue::Special(code) => {
                    if let Some(punct) = SpecialToken::from_code(*code) {
                        result.push_str(punct.spelling());
                    }
                }
                _ => {}
            }
        }
        result
    }

    /// Stringify macro argument per C99 6.10.3.2p2
    /// Unlike tokens_to_text, this properly escapes string/char literals:
    /// - Inserts delimiting quotes around string/char constants
    /// - Escapes backslashes and double-quotes within them
    fn stringify_arg(&self, tokens: &[Token], idents: &IdentTable) -> String {
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
                TokenValue::String(s)
                | TokenValue::WideString(s)
                | TokenValue::Utf16String(s)
                | TokenValue::Utf32String(s) => {
                    // The encoding prefix is part of the spelling, so it must
                    // survive stringification (C99 6.10.3.2p2).
                    result.push_str(match &token.value {
                        TokenValue::WideString(_) => "L",
                        TokenValue::Utf16String(_) => "u",
                        TokenValue::Utf32String(_) => "U",
                        _ => "",
                    });
                    // C99 6.10.3.2p2: insert \ before each " and \ including delimiters
                    result.push('\\');
                    result.push('"');
                    for ch in s.chars() {
                        if ch == '"' || ch == '\\' {
                            result.push('\\');
                        }
                        result.push(ch);
                    }
                    result.push('\\');
                    result.push('"');
                }
                TokenValue::Char(c)
                | TokenValue::WideChar(c)
                | TokenValue::Utf16Char(c)
                | TokenValue::Utf32Char(c) => {
                    result.push_str(match &token.value {
                        TokenValue::WideChar(_) => "L",
                        TokenValue::Utf16Char(_) => "u",
                        TokenValue::Utf32Char(_) => "U",
                        _ => "",
                    });
                    // C99 6.10.3.2p2: insert \ before each " and \ in char constants
                    result.push('\'');
                    for ch in c.chars() {
                        if ch == '\\' || ch == '"' {
                            result.push('\\');
                        }
                        result.push(ch);
                    }
                    result.push('\'');
                }
                TokenValue::Special(code) if *code < SpecialToken::BASE => {
                    result.push(*code as u8 as char);
                }
                // A punctuator of more than one character has a spelling too.
                // Dropping it turned `a >> b` into `a  b`, in `#x` (6.10.3.2p2
                // asks for "the spelling of the preprocessing token") and in
                // the text a `_Pragma` is rebuilt from alike.
                TokenValue::Special(code) => {
                    if let Some(punct) = SpecialToken::from_code(*code) {
                        result.push_str(punct.spelling());
                    }
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
            crate::diag::error_args(
                *macro_pos,
                "unterminated argument list invoking macro \"{0}\"",
                &[macro_name],
            );
        }

        args
    }

    /// Expand a function-like macro
    /// C17 6.10.3p4: a function-like macro invocation must supply as many
    /// arguments as the definition has parameters (and at least that many for a
    /// variadic one).
    ///
    /// Substitution used `args.get(idx).unwrap_or_default()`, so a missing
    /// argument silently became empty and an extra one was silently dropped.
    fn check_macro_arity(&self, mac: &Macro, args: &[Vec<Token>], pos: &Position) {
        // `collect_macro_args` yields no arguments at all for `F()`. That spells
        // *one empty argument* unless the macro takes none, so recover the
        // distinction here rather than in the collector, which cannot see the
        // definition.
        let supplied = if args.is_empty() {
            if mac.params.is_empty() && !mac.is_variadic {
                0
            } else {
                1
            }
        } else {
            args.len()
        };
        let required = mac.params.len();

        // The variadic part may be empty; that is a GNU extension C23 adopted,
        // and rejecting it would break a great deal of existing code.
        let wrong = if mac.is_variadic {
            supplied < required
        } else {
            supplied != required
        };
        if !wrong {
            return;
        }

        let expected = if mac.is_variadic {
            format!("at least {}", required)
        } else {
            required.to_string()
        };
        // Both forms are msgids: the singular/plural split is part of the
        // English sentence and cannot be substituted in from outside.
        diag::error_plural(
            *pos,
            "macro '{0}' requires {1} argument, but {2} given",
            "macro '{0}' requires {1} arguments, but {2} given",
            required,
            &[&mac.name, &expected, &supplied.to_string()],
        );
    }

    fn expand_function_macro(
        &mut self,
        mac: &Macro,
        args: &[Vec<Token>],
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        self.check_macro_arity(mac, args, pos);

        // Note: Don't add to expanding set yet - arguments need to be fully expanded first.
        // The expanding set is only used during the rescan phase to prevent infinite recursion.

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
                    // Stringify the argument per C99 6.10.3.2p2
                    let arg = args.get(*idx).cloned().unwrap_or_default();
                    let text = self.stringify_arg(&arg, idents);
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
                            // Re-point the token at the invocation for
                            // diagnostics, but keep whether it was preceded by
                            // white space: that belongs to the argument's own
                            // spelling, and 6.10.3.2p2 makes `#` reproduce it.
                            // Taking `whitespace` from the invocation site gave
                            // every expanded token one, so the two-level
                            // `XSTR(x)`/`STR(x)` idiom turned `1+2` into
                            // `"1 + 2"` while the direct `STR(1+2)` was right.
                            let whitespace = tok.pos.whitespace;
                            tok.pos = *pos;
                            tok.pos.newline = false;
                            tok.pos.whitespace = whitespace;
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
        // NOTE: Only add to expanding set during rescan to prevent infinite recursion
        // Arguments were already expanded above, so this only affects the replacement list rescan
        self.expanding.insert(mac.name.clone());
        let rescanned = self.preprocess(result, idents);
        self.expanding.remove(&mac.name);

        let mut filtered: Vec<_> = rescanned
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .collect();

        // Mark any occurrences of this macro's name in the result as "blue painted"
        // per C99 6.10.3.4: if the macro name appears in its own expansion, it should
        // not be re-expanded in subsequent contexts.
        let mac_name_id = idents.intern(&mac.name);
        for tok in &mut filtered {
            if let TokenValue::Ident(id) = &tok.value {
                if *id == mac_name_id {
                    tok.mark_no_expand(&mac.name);
                }
            }
        }

        // The FIRST token of a macro expansion should inherit whitespace
        // from the macro invocation position, not the macro body.
        // This ensures "MACRO(args)" expands with proper spacing.
        if let Some(first) = filtered.first_mut() {
            first.pos.whitespace = pos.whitespace;
        }

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

        // Note: Don't add to expanding set yet - that happens during rescan phase only.

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
        // NOTE: Only add to expanding set during rescan to prevent infinite recursion
        // (e.g., when const -> __const and __const -> const both exist)
        self.expanding.insert(mac.name.clone());
        let rescanned = self.preprocess(result, idents);
        self.expanding.remove(&mac.name);

        let mut filtered: Vec<_> = rescanned
            .into_iter()
            .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
            .collect();

        // Mark any occurrences of this macro's name in the result as "blue painted"
        // per C99 6.10.3.4: if the macro name appears in its own expansion, it should
        // not be re-expanded in subsequent contexts.
        let mac_name_id = idents.intern(&mac.name);
        for tok in &mut filtered {
            if let TokenValue::Ident(id) = &tok.value {
                if *id == mac_name_id {
                    tok.mark_no_expand(&mac.name);
                }
            }
        }

        // The FIRST token of a macro expansion should inherit whitespace
        // from the macro invocation position, not the macro body.
        // This ensures "extern __inline" expands to "extern inline" with space.
        if let Some(first) = filtered.first_mut() {
            first.pos.whitespace = pos.whitespace;
        }

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
            // The macro body collapses every string/char encoding into one
            // variant, so the original token type is what recovers it.
            MacroTokenValue::String(s) => match mt.typ {
                TokenType::WideString => TokenValue::WideString(s.clone()),
                TokenType::Utf16String => TokenValue::Utf16String(s.clone()),
                TokenType::Utf32String => TokenValue::Utf32String(s.clone()),
                _ => TokenValue::String(s.clone()),
            },
            MacroTokenValue::Char(c) => match mt.typ {
                TokenType::WideChar => TokenValue::WideChar(c.clone()),
                TokenType::Utf16Char => TokenValue::Utf16Char(c.clone()),
                TokenType::Utf32Char => TokenValue::Utf32Char(c.clone()),
                _ => TokenValue::Char(c.clone()),
            },
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
            BuiltinMacro::Line => {
                let effective_line = (pos.line as i32 + self.line_offset) as u32;
                Some(vec![Token::with_value(
                    TokenType::Number,
                    *pos,
                    TokenValue::Number(effective_line.to_string()),
                )])
            }
            BuiltinMacro::File => {
                let effective_file = self
                    .line_file_override
                    .as_ref()
                    .unwrap_or(&self.current_file)
                    .clone();
                Some(vec![Token::with_value(
                    TokenType::String,
                    *pos,
                    TokenValue::String(effective_file),
                )])
            }
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

        // Try to get the first token from the argument list
        let first_tok = match args[0].first() {
            Some(tok) => tok,
            None => return false,
        };

        // Try to get StringId directly for O(1) tag-based lookup
        let arg_id = if let TokenValue::Ident(id) = &first_tok.value {
            Some(*id)
        } else {
            None
        };

        match builtin {
            BuiltinMacro::HasAttribute => {
                if let Some(id) = arg_id {
                    crate::kw::has_tag(id, crate::kw::SUPPORTED_ATTR)
                } else {
                    // One table decides this. A second hardcoded list here
                    // had already drifted from it in both directions.
                    let name = self.token_to_string(first_tok, idents);
                    idents
                        .lookup(&name)
                        .is_some_and(|id| crate::kw::has_tag(id, crate::kw::SUPPORTED_ATTR))
                }
            }
            BuiltinMacro::HasBuiltin => {
                if let Some(id) = arg_id {
                    crate::builtins::is_builtin_id(id)
                } else {
                    let name = self.token_to_string(first_tok, idents);
                    crate::builtins::is_builtin(name.as_str())
                }
            }
            BuiltinMacro::HasFeature | BuiltinMacro::HasExtension => {
                let name = self.token_to_string(first_tok, idents);
                // Return true for features/extensions we implement
                matches!(
                    name.as_str(),
                    // GNU extensions
                    "statement_expressions" |
                    "statement_expressions_in_macros" |
                    "gnu_asm" |
                    // C11 features
                    "c_atomic" |
                    "c_static_assert" |
                    "c_alignas" |
                    "c_alignof" |
                    "c_thread_local" |
                    "c_generic_selections"
                )
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

/// A value in a `#if` expression.
///
/// C17 6.10.1p4 requires `#if` arithmetic to be done in `intmax_t` and
/// `uintmax_t`, which is 64-bit on every target here. Both domains are carried
/// in an `i128` so a `u64` value is representable exactly and *signed*
/// comparison of the carrier is already the correct unsigned comparison once
/// both sides have been promoted. Results are wrapped back to 64 bits after
/// every operation, so overflow behaves as it does in the target's arithmetic.
///
/// This replaces a plain `i64` that silently mapped anything out of range to
/// `0`, so `#if 0xFFFFFFFFFFFFFFFF` took the false branch and `__UINT64_MAX__`
/// evaluated as zero.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PpValue {
    /// The value: a sign-extended i64 when signed, a zero-extended u64 when not.
    v: i128,
    unsigned: bool,
}

impl PpValue {
    fn signed(v: i128) -> Self {
        PpValue {
            v: v as i64 as i128,
            unsigned: false,
        }
    }

    fn unsigned(v: i128) -> Self {
        PpValue {
            v: v as u64 as i128,
            unsigned: true,
        }
    }

    fn from_parts(v: i128, unsigned: bool) -> Self {
        if unsigned {
            Self::unsigned(v)
        } else {
            Self::signed(v)
        }
    }

    fn is_true(self) -> bool {
        self.v != 0
    }

    /// The carrier, for operations that do not apply the usual conversions.
    fn raw(self) -> i128 {
        self.v
    }

    /// Apply the usual arithmetic conversions: if either operand is unsigned,
    /// both are taken in the unsigned domain and so is the result.
    fn promote(a: PpValue, b: PpValue) -> (i128, i128, bool) {
        if a.unsigned || b.unsigned {
            (a.v as u64 as i128, b.v as u64 as i128, true)
        } else {
            (a.v, b.v, false)
        }
    }
}

struct ExprEvaluator<'a, 'b> {
    pp: &'a Preprocessor<'b>,
    idents: &'a IdentTable,
    tokens: Vec<Token>,
    pos: usize,
    /// Set while parsing a short-circuited operand, whose diagnostics must not
    /// fire — the whole point of `&&`/`||` not evaluating that side.
    suppressed: bool,
}

impl<'a, 'b> ExprEvaluator<'a, 'b> {
    fn new(pp: &'a Preprocessor<'b>, idents: &'a IdentTable) -> Self {
        Self {
            pp,
            idents,
            tokens: Vec::new(),
            pos: 0,
            suppressed: false,
        }
    }

    fn evaluate(&mut self, tokens: &[Token]) -> PpValue {
        self.tokens = tokens.to_vec();
        self.pos = 0;
        self.expr_ternary()
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
    // ?: || && | ^ & ==/!= relational shift additive multiplicative unary

    /// Ternary operator has lowest precedence: cond ? true_val : false_val
    fn expr_ternary(&mut self) -> PpValue {
        let cond = self.expr_or();
        if self.is_special(b'?' as u32) {
            self.advance();
            let true_val = self.expr_ternary();
            if self.is_special(b':' as u32) {
                self.advance();
            } else {
                let pos = self.current().map(|t| t.pos).unwrap_or_default();
                diag::error(pos, &gettext("expected ':' in conditional expression"));
            }
            let false_val = self.expr_ternary();
            if cond.is_true() {
                true_val
            } else {
                false_val
            }
        } else {
            cond
        }
    }

    fn expr_or(&mut self) -> PpValue {
        let mut left = self.expr_and();
        while self.is_special(SpecialToken::LogicalOr as u32) {
            self.advance();
            // C17 6.5.14p4: the right operand is not evaluated if the left
            // compares unequal to 0. Parsing must still consume it.
            if left.is_true() {
                self.skip_expr_and();
                left = PpValue::signed(1);
            } else {
                let right = self.expr_and();
                left = PpValue::signed(i128::from(right.is_true()));
            }
        }
        left
    }

    fn expr_and(&mut self) -> PpValue {
        let mut left = self.expr_bitor();
        while self.is_special(SpecialToken::LogicalAnd as u32) {
            self.advance();
            // C17 6.5.13p4: the right operand is not evaluated if the left
            // compares equal to 0.
            if left.is_true() {
                let right = self.expr_bitor();
                left = PpValue::signed(i128::from(right.is_true()));
            } else {
                self.skip_expr_bitor();
                left = PpValue::signed(0);
            }
        }
        left
    }

    /// Parse and discard an `&&` operand, for the short-circuited case.
    fn skip_expr_and(&mut self) {
        let saved = self.suppressed;
        self.suppressed = true;
        let _ = self.expr_and();
        self.suppressed = saved;
    }

    /// Parse and discard a `||` operand, for the short-circuited case.
    fn skip_expr_bitor(&mut self) {
        let saved = self.suppressed;
        self.suppressed = true;
        let _ = self.expr_bitor();
        self.suppressed = saved;
    }

    fn expr_bitor(&mut self) -> PpValue {
        let mut left = self.expr_bitxor();
        while self.is_special(b'|' as u32) {
            self.advance();
            let right = self.expr_bitxor();
            let (a, b, u) = PpValue::promote(left, right);
            left = PpValue::from_parts(a | b, u);
        }
        left
    }

    fn expr_bitxor(&mut self) -> PpValue {
        let mut left = self.expr_bitand();
        while self.is_special(b'^' as u32) {
            self.advance();
            let right = self.expr_bitand();
            let (a, b, u) = PpValue::promote(left, right);
            left = PpValue::from_parts(a ^ b, u);
        }
        left
    }

    fn expr_bitand(&mut self) -> PpValue {
        let mut left = self.expr_equality();
        while self.is_special(b'&' as u32) {
            self.advance();
            let right = self.expr_equality();
            let (a, b, u) = PpValue::promote(left, right);
            left = PpValue::from_parts(a & b, u);
        }
        left
    }

    fn expr_equality(&mut self) -> PpValue {
        let mut left = self.expr_relational();
        loop {
            if self.is_special(SpecialToken::Equal as u32) {
                self.advance();
                let right = self.expr_relational();
                let (a, b, _) = PpValue::promote(left, right);
                left = PpValue::signed(i128::from(a == b));
            } else if self.is_special(SpecialToken::NotEqual as u32) {
                self.advance();
                let right = self.expr_relational();
                let (a, b, _) = PpValue::promote(left, right);
                left = PpValue::signed(i128::from(a != b));
            } else {
                break;
            }
        }
        left
    }

    fn expr_relational(&mut self) -> PpValue {
        // `promote` puts both operands in the same domain first, so a plain
        // i128 comparison is already the unsigned one when either side is
        // unsigned — that is the point of carrying values in the wider type.
        const LT: u32 = b'<' as u32;
        const GT: u32 = b'>' as u32;
        let lte = SpecialToken::Lte as u32;
        let gte = SpecialToken::Gte as u32;

        let mut left = self.expr_shift();
        loop {
            let op = [LT, GT, lte, gte].into_iter().find(|&t| self.is_special(t));
            let Some(op) = op else { break };
            self.advance();
            let right = self.expr_shift();
            let (a, b, _) = PpValue::promote(left, right);
            let result = match op {
                LT => a < b,
                GT => a > b,
                t if t == lte => a <= b,
                _ => a >= b,
            };
            left = PpValue::signed(i128::from(result));
        }
        left
    }

    fn expr_shift(&mut self) -> PpValue {
        let mut left = self.expr_additive();
        loop {
            let is_left = self.is_special(SpecialToken::LeftShift as u32);
            if !is_left && !self.is_special(SpecialToken::RightShift as u32) {
                break;
            }
            self.advance();
            let right = self.expr_additive();
            // A shift does not apply the usual arithmetic conversions: the
            // result takes the left operand's type (C17 6.5.7p3). A count
            // outside [0, 64) is undefined; clamp rather than panic.
            let count = right.v.clamp(0, 63) as u32;
            let v = if is_left {
                left.raw() << count
            } else if left.unsigned {
                ((left.raw() as u64) >> count) as i128
            } else {
                (left.raw() as i64 >> count) as i128
            };
            left = PpValue::from_parts(v, left.unsigned);
        }
        left
    }

    fn expr_additive(&mut self) -> PpValue {
        let mut left = self.expr_multiplicative();
        loop {
            let is_add = self.is_special(b'+' as u32);
            if !is_add && !self.is_special(b'-' as u32) {
                break;
            }
            self.advance();
            let right = self.expr_multiplicative();
            let (a, b, u) = PpValue::promote(left, right);
            let v = if is_add {
                a.wrapping_add(b)
            } else {
                a.wrapping_sub(b)
            };
            left = PpValue::from_parts(v, u);
        }
        left
    }

    fn expr_multiplicative(&mut self) -> PpValue {
        let mut left = self.expr_unary();
        loop {
            let op = if self.is_special(b'*' as u32) {
                b'*'
            } else if self.is_special(b'/' as u32) {
                b'/'
            } else if self.is_special(b'%' as u32) {
                b'%'
            } else {
                break;
            };
            // Capture the operator's own position: by the time the divisor is
            // known, `current()` has moved past the end of the expression.
            let op_pos = self.current().map(|t| t.pos).unwrap_or_default();
            self.advance();
            let right = self.expr_unary();
            let (a, b, u) = PpValue::promote(left, right);
            let v = match op {
                b'*' => a.wrapping_mul(b),
                _ if b == 0 => {
                    // Only diagnose when this operand is actually reached; a
                    // short-circuited `#if defined(X) && 1/X` must stay quiet.
                    if !self.suppressed {
                        diag::error(
                            op_pos,
                            &gettext("division by zero in preprocessor expression"),
                        );
                    }
                    0
                }
                b'/' => a.wrapping_div(b),
                _ => a.wrapping_rem(b),
            };
            left = PpValue::from_parts(v, u);
        }
        left
    }

    fn expr_unary(&mut self) -> PpValue {
        if self.is_special(b'!' as u32) {
            self.advance();
            let val = self.expr_unary();
            return PpValue::signed(i128::from(!val.is_true()));
        }
        if self.is_special(b'~' as u32) {
            self.advance();
            let val = self.expr_unary();
            return PpValue::from_parts(!val.raw(), val.unsigned);
        }
        if self.is_special(b'-' as u32) {
            self.advance();
            let val = self.expr_unary();
            return PpValue::from_parts(val.raw().wrapping_neg(), val.unsigned);
        }
        if self.is_special(b'+' as u32) {
            self.advance();
            return self.expr_unary();
        }
        self.expr_primary()
    }

    fn expr_primary(&mut self) -> PpValue {
        // Handle defined(X) or defined X
        if self.is_ident("defined") {
            self.advance();
            return PpValue::signed(self.eval_defined() as i128);
        }

        // Handle __has_attribute(X)
        if self.is_ident("__has_attribute") {
            self.advance();
            return PpValue::signed(self.eval_has_attribute() as i128);
        }

        // Handle __has_builtin(X)
        if self.is_ident("__has_builtin") {
            self.advance();
            return PpValue::signed(self.eval_has_builtin_expr() as i128);
        }

        // Handle __has_feature(X) and __has_extension(X)
        if self.is_ident("__has_feature") || self.is_ident("__has_extension") {
            self.advance();
            return PpValue::signed(self.eval_has_feature() as i128);
        }

        // Handle parenthesized expression
        if self.is_special(b'(' as u32) {
            self.advance();
            let val = self.expr_ternary();
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

        // Handle character literal (any encoding prefix: L'x', u'x', U'x')
        if let Some(tok) = self.current() {
            let char_str = match &tok.value {
                TokenValue::Char(c)
                | TokenValue::WideChar(c)
                | TokenValue::Utf16Char(c)
                | TokenValue::Utf32Char(c) => Some(c.clone()),
                _ => None,
            };
            if let Some(char_str) = char_str {
                self.advance();
                if char_str.is_empty() {
                    return PpValue::signed(0);
                }
                // Pack all chars big-endian (GCC-compatible)
                let mut val: i64 = 0;
                for c in char_str.chars() {
                    val = (val << 8) | (c as i64);
                }
                return PpValue::signed(val as i128);
            }
        }

        // Handle identifier - after macro expansion, any remaining identifier
        // is undefined and should evaluate to 0 (per C standard)
        if let Some(tok) = self.current() {
            if matches!(&tok.value, TokenValue::Ident(_)) {
                self.advance();
                return PpValue::signed(0);
            }
        }

        PpValue::signed(0)
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

        if self
            .idents
            .lookup(&name)
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::SUPPORTED_ATTR))
        {
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

        // Use centralized builtin registry
        if crate::builtins::is_builtin(name.as_str()) {
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

        // Return 1 for features/extensions we implement
        let supported = matches!(
            name.as_str(),
            // GNU extensions
            "statement_expressions" | // GNU ({ }) extension
            "statement_expressions_in_macros" |
            "gnu_asm" |
            // C11 features
            "c_atomic" |
            "c_static_assert" |
            "c_alignas" |
            "c_alignof" |
            "c_thread_local" |
            "c_generic_selections"
        );

        if supported {
            1
        } else {
            0
        }
    }

    /// Parse an integer preprocessing token into a `PpValue`.
    ///
    /// The value is unsigned if it carries a `u`/`U` suffix, or if it does not
    /// fit in `intmax_t` but does fit in `uintmax_t` (C17 6.4.4.1p5). Parsing
    /// into `u64` first is what makes `#if 0xFFFFFFFFFFFFFFFF` work; the old
    /// `i64::from_str_radix(...).unwrap_or(0)` turned every such constant into
    /// a silent zero.
    fn parse_number(&self, s: &str) -> PpValue {
        let digits = s.trim_end_matches(['u', 'U', 'l', 'L']);
        let suffix = &s[digits.len()..];
        let suffix_unsigned = suffix.contains('u') || suffix.contains('U');

        let (body, radix) = if let Some(hex) = digits
            .strip_prefix("0x")
            .or_else(|| digits.strip_prefix("0X"))
        {
            (hex, 16)
        } else if let Some(bin) = digits
            .strip_prefix("0b")
            .or_else(|| digits.strip_prefix("0B"))
        {
            (bin, 2)
        } else if digits.len() > 1
            && digits.starts_with('0')
            && digits[1..].starts_with(|c: char| c.is_ascii_digit())
        {
            (&digits[1..], 8)
        } else {
            (digits, 10)
        };

        match u64::from_str_radix(body, radix) {
            Ok(v) => {
                // Too large for intmax_t means the constant's type is
                // uintmax_t, even without a suffix.
                let unsigned = suffix_unsigned || v > i64::MAX as u64;
                PpValue::from_parts(v as i128, unsigned)
            }
            Err(_) => PpValue::signed(0),
        }
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Where the system headers are, and what the command line added to that.
///
/// Separate from [`PreprocessConfig`] because the preprocessor needs it at
/// construction, before any other option is applied.
#[derive(Debug, Clone, Default)]
pub struct SystemSearch<'a> {
    /// `--sysroot`: the target's directories are read from under this prefix.
    pub sysroot: Option<&'a str>,
    /// `-isystem`: system directories searched ahead of the target's own.
    pub isystem: &'a [String],
    /// `-idirafter`: system directories searched behind the target's own.
    pub idirafter: &'a [String],
}

/// Configuration for preprocessing command-line options
#[derive(Default)]
pub struct PreprocessConfig<'a> {
    /// Command-line -D defines
    pub defines: &'a [String],
    /// Command-line -U undefines
    pub undefines: &'a [String],
    /// Command-line -I include paths
    pub include_paths: &'a [String],
    /// Where the system headers are; see [`SystemSearch`].
    pub search: SystemSearch<'a>,
    /// If true, disable system include paths (-nostdinc)
    pub no_std_inc: bool,
    /// If true, disable builtin headers (-nobuiltininc)
    pub no_builtin_inc: bool,
    /// If true, apply translation phase 1 trigraph replacement (--trigraphs).
    pub trigraphs: bool,
    /// If true, the input is a `.i` operand -- already the output of `c17 -E`
    /// -- and the processing that produced it must not be repeated.
    pub preprocessed: bool,
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
    let mut pp = Preprocessor::new(target, filename, &config.search);

    // -nostdinc drops the bundled headers as well as the system directories,
    // which is what gcc does: `gcc -nostdinc` cannot find <stddef.h> either,
    // its own include directory being one of the standard ones. Probed rather
    // than assumed -- this was very nearly "aligned" the other way.
    if config.no_std_inc {
        pp.use_system_headers = false;
        pp.use_builtin_headers = false;
    }
    if config.no_builtin_inc {
        pp.use_builtin_headers = false;
    }
    pp.trigraphs = config.trigraphs;
    pp.preprocessed = config.preprocessed;

    // Add -I include paths
    for path in config.include_paths {
        pp.quote_include_paths.push(path.clone());
    }

    // Process -D defines
    for def in config.defines {
        pp.define_from_cmdline(def, idents);
    }

    // Process -U undefines
    for undef in config.undefines {
        pp.undef_macro(undef);
    }

    pp.preprocess(tokens, idents)
}

// ============================================================================
// Assembly File Preprocessing
// ============================================================================

/// Configuration for assembly file preprocessing
#[derive(Debug, Clone, Default)]
pub struct AsmPreprocessConfig<'a> {
    /// Command-line -D defines
    pub defines: &'a [String],
    /// Command-line -U undefines
    pub undefines: &'a [String],
    /// Command-line -I include paths
    pub include_paths: &'a [String],
    /// Where the system headers are; see [`SystemSearch`].
    pub search: SystemSearch<'a>,
    /// If true, disable system include paths (-nostdinc)
    pub no_std_inc: bool,
}

/// Preprocess an assembly file (.S) and return the preprocessed text.
///
/// This uses the same preprocessor as C files but with assembly-specific
/// comment syntax (`;` for line comments, no `//` or `/* */`).
///
/// # Arguments
/// * `content` - Raw bytes of the assembly source file
/// * `target` - Target platform configuration
/// * `filename` - Name of the source file (for `__FILE__` and diagnostics)
/// * `config` - Preprocessing configuration (defines, undefines, include paths)
///
/// # Returns
/// The preprocessed assembly text as a string
pub fn preprocess_asm_file(
    content: &[u8],
    target: &Target,
    filename: &str,
    config: &AsmPreprocessConfig<'_>,
) -> String {
    // Create string table for tokenization
    let mut strings = IdentTable::new();

    // Initialize stream for this file
    let stream_id = diag::init_stream(filename);

    // Tokenize with assembly mode (`;` comments)
    let tokens = {
        let mut tokenizer =
            Tokenizer::new_with_mode(content, stream_id, &mut strings, LexerMode::Assembly);
        tokenizer.tokenize()
    };

    // Create preprocessor with assembly-specific predefined macros. The C
    // standard macros are undefined immediately below, so nothing the language
    // mode contributes survives into an assembly translation unit.
    let mut pp = Preprocessor::new(target, filename, &config.search);

    // Use assembly lexer mode for included files as well
    pp.lexer_mode = LexerMode::Assembly;

    // Undefine C-specific macros that don't apply to assembly
    pp.undef_macro("__STDC__");
    pp.undef_macro("__STDC_VERSION__");
    pp.undef_macro("__STDC_HOSTED__");

    // Define __ASSEMBLER__ (GCC-compatible, indicates assembly preprocessing)
    pp.define_macro(Macro::predefined("__ASSEMBLER__", Some("1")));

    // Handle -nostdinc flag
    if config.no_std_inc {
        pp.use_system_headers = false;
        pp.use_builtin_headers = false;
    }

    // Add -I include paths
    for path in config.include_paths {
        pp.quote_include_paths.push(path.clone());
    }

    // Process -D defines
    for def in config.defines {
        pp.define_from_cmdline(def, &mut strings);
    }

    // Process -U undefines
    for undef in config.undefines {
        pp.undef_macro(undef);
    }

    // Preprocess
    let preprocessed = pp.preprocess(tokens, &mut strings);

    // Convert tokens back to text
    tokens_to_text(&preprocessed, &strings)
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
        let pp = Preprocessor::new(&target, "test.c", &SystemSearch::default());
        assert!(pp.is_defined("__STDC__"));
        assert!(pp.is_defined("__STDC_VERSION__"));
    }

    #[test]
    fn test_predefined_arch() {
        let target = Target::host();
        let pp = Preprocessor::new(&target, "test.c", &SystemSearch::default());

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
    fn test_line_directive_consumed() {
        // #line should be consumed and not pass through as tokens
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
        // An incompatible redefinition is diagnosed (C17 6.10.3p2) but is not
        // fatal: the standard requires only a diagnostic, and rejecting would
        // break a great deal of code that redefines a macro benignly. The
        // later definition wins, as it always has.
        //
        // This test used to assert the silent override *as intended*, which is
        // why the constraint went unimplemented.
        let input = "#define X 1\n#define X 2\nX";
        let (tokens, idents) = preprocess_str(input);
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"2".to_string()));
        assert!(!strs.contains(&"1".to_string()));
    }

    #[test]
    fn test_macro_redefinition_conflict_detection() {
        // Build a macro as if it came from a `#define` directive: the
        // implementation-predefined flag exempts a macro from the constraint,
        // which is not what is under test here.
        fn obj(name: &str, value: &str) -> Macro {
            let mut m = Macro::predefined(name, Some(value));
            m.predefined = false;
            m
        }

        // Identical replacement lists are permitted.
        assert!(macro_redefinition_conflict(&obj("A", "1"), &obj("A", "1")).is_none());
        // Differing ones are not.
        assert!(macro_redefinition_conflict(&obj("A", "1"), &obj("A", "2")).is_some());

        // Object-like versus function-like.
        let mut fnlike = obj("A", "1");
        fnlike.is_function = true;
        fnlike.params = vec![MacroParam {
            name: "x".into(),
            index: 0,
        }];
        assert!(macro_redefinition_conflict(&obj("A", "1"), &fnlike).is_some());

        // Same shape, differently spelled parameters.
        let mut renamed = fnlike.clone();
        renamed.params = vec![MacroParam {
            name: "y".into(),
            index: 0,
        }];
        assert!(macro_redefinition_conflict(&fnlike, &renamed).is_some());

        // A parameter list that matches is fine.
        assert!(macro_redefinition_conflict(&fnlike, &fnlike.clone()).is_none());
    }

    #[test]
    fn test_replacement_lists_ignore_leading_whitespace() {
        // Whitespace before the first replacement token is not a separation
        // *within* the list. Without this, every compilation against glibc
        // warned: we predefine __GLIBC__ with no leading space, while
        // features.h writes `#define __GLIBC__ 2` with one.
        let a = vec![MacroToken {
            typ: TokenType::Number,
            value: MacroTokenValue::Number("2".into()),
            whitespace: false,
        }];
        let b = vec![MacroToken {
            typ: TokenType::Number,
            value: MacroTokenValue::Number("2".into()),
            whitespace: true,
        }];
        assert!(replacement_lists_identical(&a, &b));

        // But whitespace between tokens still counts.
        let two = |ws: bool| {
            vec![
                MacroToken {
                    typ: TokenType::Number,
                    value: MacroTokenValue::Number("1".into()),
                    whitespace: false,
                },
                MacroToken {
                    typ: TokenType::Number,
                    value: MacroTokenValue::Number("2".into()),
                    whitespace: ws,
                },
            ]
        };
        assert!(!replacement_lists_identical(&two(true), &two(false)));
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

    // ========================================================================
    // Include guard detection tests
    // ========================================================================

    #[test]
    fn test_detect_include_guard_ifndef_define() {
        // Standard include guard pattern: #ifndef X / #define X
        let content = b"#ifndef FOO_H\n#define FOO_H\n// content\n#endif\n";
        assert_eq!(
            Preprocessor::detect_include_guard(content),
            Some("FOO_H".to_string())
        );
    }

    #[test]
    fn test_detect_include_guard_with_leading_comment() {
        // Include guard with leading block comment
        let content = b"/* Header file */\n#ifndef MY_HEADER_H\n#define MY_HEADER_H\n#endif\n";
        assert_eq!(
            Preprocessor::detect_include_guard(content),
            Some("MY_HEADER_H".to_string())
        );
    }

    #[test]
    fn test_detect_include_guard_with_line_comment() {
        // Include guard with leading line comment
        let content = b"// Header file\n#ifndef GUARD_H\n#define GUARD_H\n#endif\n";
        assert_eq!(
            Preprocessor::detect_include_guard(content),
            Some("GUARD_H".to_string())
        );
    }

    #[test]
    fn test_detect_include_guard_no_define() {
        // Not a guard - #ifndef without matching #define
        let content = b"#ifndef FOO_H\n#error \"Use other header\"\n#endif\n";
        assert_eq!(Preprocessor::detect_include_guard(content), None);
    }

    #[test]
    fn test_detect_include_guard_different_macro() {
        // Not a guard - #define defines different macro
        let content = b"#ifndef FOO_H\n#define BAR_H\n#endif\n";
        assert_eq!(Preprocessor::detect_include_guard(content), None);
    }

    #[test]
    fn test_detect_include_guard_if_not_defined() {
        // Alternative pattern: #if !defined(X)
        let content = b"#if !defined(MYGUARD)\n#define MYGUARD\n#endif\n";
        assert_eq!(
            Preprocessor::detect_include_guard(content),
            Some("MYGUARD".to_string())
        );
    }

    #[test]
    fn test_detect_include_guard_no_guard() {
        // Not a guard - just regular code
        let content = b"int x = 1;\n";
        assert_eq!(Preprocessor::detect_include_guard(content), None);
    }

    #[test]
    fn test_detect_include_guard_conditional_default_with_content() {
        // NOT a guard - conditional default definition followed by more content
        // This pattern: #ifndef X / #define X default / #endif / more macros
        let content = b"#ifndef FOO\n#define FOO 1\n#endif\n#define BAR 2\n";
        assert_eq!(Preprocessor::detect_include_guard(content), None);
    }

    #[test]
    fn test_detect_include_guard_if_defined_conditional_default() {
        // NOT a guard - #if !defined() pattern with content after #endif
        let content =
            b"#if !defined(DEFAULT_VAL)\n#define DEFAULT_VAL 42\n#endif\n#define OTHER 1\n";
        assert_eq!(Preprocessor::detect_include_guard(content), None);
    }

    // ========================================================================
    // bool/true/false predefined macro tests
    // ========================================================================

    #[test]
    fn test_bool_macro_expands_to_bool_with_stdbool() {
        // bool should expand to _Bool after including stdbool.h
        let (tokens, idents) = preprocess_str("#include <stdbool.h>\nbool x;");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"_Bool".to_string()));
    }

    #[test]
    fn test_true_macro_expands_to_1_with_stdbool() {
        // true should expand to 1 after including stdbool.h
        let (tokens, idents) = preprocess_str("#include <stdbool.h>\nint x = true;");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"1".to_string()));
    }

    #[test]
    fn test_false_macro_expands_to_0_with_stdbool() {
        // false should expand to 0 after including stdbool.h
        let (tokens, idents) = preprocess_str("#include <stdbool.h>\nint x = false;");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"0".to_string()));
    }

    #[test]
    fn test_bool_not_predefined_without_stdbool() {
        // bool should NOT be predefined without stdbool.h (matches GCC/Clang)
        let (tokens, idents) = preprocess_str("bool x;");
        let strs = get_token_strings(&tokens, &idents);
        // bool should remain unexpanded (as an identifier)
        assert!(strs.contains(&"bool".to_string()));
        assert!(!strs.contains(&"_Bool".to_string()));
    }

    // ========================================================================
    // Blue-painting tests (C99 6.10.3.4 - recursive macro prevention)
    // ========================================================================

    #[test]
    fn test_blue_painting_object_like_macro() {
        // A macro that expands to itself should not infinitely recurse
        // FOO expands to "FOO + 1", but the FOO in the expansion should not re-expand
        let (tokens, idents) = preprocess_str("#define FOO FOO + 1\nFOO");
        let strs = get_token_strings(&tokens, &idents);
        // Should get "FOO + 1", not infinite recursion
        assert!(strs.contains(&"FOO".to_string()));
        assert!(strs.contains(&"+".to_string()));
        assert!(strs.contains(&"1".to_string()));
    }

    #[test]
    fn test_blue_painting_function_like_macro() {
        // Function-like macro that references itself
        let (tokens, idents) = preprocess_str("#define F(x) F(x + 1)\nF(0)");
        let strs = get_token_strings(&tokens, &idents);
        // Should get "F(0 + 1)", the inner F should not re-expand
        assert!(strs.contains(&"F".to_string()));
        assert!(strs.contains(&"0".to_string()));
        assert!(strs.contains(&"+".to_string()));
        assert!(strs.contains(&"1".to_string()));
    }

    #[test]
    fn test_blue_painting_indirect_recursion() {
        // A -> B -> A should not infinitely recurse
        let (tokens, idents) = preprocess_str("#define A B\n#define B A\nA");
        let strs = get_token_strings(&tokens, &idents);
        // A -> B -> A (blue painted, stops)
        // Result should contain "A"
        assert!(strs.contains(&"A".to_string()));
    }

    // ========================================================================
    // Predefined macro tokenization tests
    // ========================================================================

    #[test]
    fn test_predefined_macro_tokenization_parentheses() {
        // Predefined macros like __DBL_MIN_EXP__ have values like "(-1021)"
        // These should be tokenized as separate tokens: (, -, 1021, )
        let mac = Macro::predefined("__TEST__", Some("(-1021)"));
        assert_eq!(mac.body.len(), 4); // (, -, 1021, )
        assert_eq!(mac.body[0].typ, TokenType::Special); // (
        assert_eq!(mac.body[1].typ, TokenType::Special); // -
        assert_eq!(mac.body[2].typ, TokenType::Number); // 1021
        assert_eq!(mac.body[3].typ, TokenType::Special); // )
    }

    #[test]
    fn test_predefined_macro_tokenization_simple_number() {
        // Simple numeric value should be a single token
        let mac = Macro::predefined("__TEST__", Some("42"));
        assert_eq!(mac.body.len(), 1);
        assert_eq!(mac.body[0].typ, TokenType::Number);
    }

    #[test]
    fn test_predefined_macro_tokenization_float() {
        // Float with exponent
        let mac = Macro::predefined("__TEST__", Some("1.0e-37"));
        assert_eq!(mac.body.len(), 1);
        assert_eq!(mac.body[0].typ, TokenType::Number);
    }

    #[test]
    fn test_predefined_macro_expansion_with_parens() {
        // When a predefined macro with parenthesized value is used, it should expand correctly
        let code = "#define __TEST__ (-1021)\nint x = __TEST__;";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        // Should contain the individual tokens
        assert!(strs.contains(&"(".to_string()));
        assert!(strs.contains(&"-".to_string()));
        assert!(strs.contains(&"1021".to_string()));
        assert!(strs.contains(&")".to_string()));
    }

    // ========================================================================
    // Wide string/char in macro expansion tests
    // ========================================================================

    #[test]
    fn test_wide_string_in_macro() {
        let (tokens, _) = preprocess_str("#define WSTR L\"hello\"\nWSTR");
        // Find the wide string token
        let wide_string_count = tokens
            .iter()
            .filter(|t| t.typ == TokenType::WideString)
            .count();
        assert_eq!(wide_string_count, 1, "should have one wide string token");
    }

    #[test]
    fn test_wide_char_in_macro() {
        let (tokens, _) = preprocess_str("#define WCHAR L'x'\nWCHAR");
        // Find the wide char token
        let wide_char_count = tokens
            .iter()
            .filter(|t| t.typ == TokenType::WideChar)
            .count();
        assert_eq!(wide_char_count, 1, "should have one wide char token");
    }

    // ========================================================================
    // Stringify, paste, include, and #if edge-case tests
    // ========================================================================

    #[test]
    fn test_stringify_empty_va_args() {
        // #__VA_ARGS__ with zero variadic args should produce an empty string ""
        let code = "#define S(...) #__VA_ARGS__\nS()";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"\"\"".to_string()),
            "expected empty string \"\\\"\\\"\", got: {:?}",
            strs
        );
    }

    #[test]
    fn test_include_macro_expanded_filename() {
        // The preprocessor should macro-expand the argument to #include.
        // Use angle-bracket include via a macro-expanded name.
        // stdbool.h is a builtin header that defines bool as _Bool.
        let code = "#include <stdbool.h>\n#define MYBOOL bool\nMYBOOL x;";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        // After #include <stdbool.h>, bool is defined as _Bool.
        // The macro MYBOOL expands to bool, which then expands to _Bool.
        assert!(
            strs.contains(&"_Bool".to_string()),
            "expected _Bool from macro chain through stdbool.h, got: {:?}",
            strs
        );
    }

    #[test]
    fn test_if_multichar_constant() {
        // Multi-character constants pack big-endian: 'ab' == ('a'<<8)+'b'
        let code = "#if 'ab' == (('a'<<8)+'b')\nyes\n#else\nno\n#endif";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"yes".to_string()),
            "expected 'yes' for multi-char constant packing, got: {:?}",
            strs
        );
    }

    #[test]
    fn test_paste_empty_arg() {
        // When the first argument is empty, a##b should produce just "hello".
        let code = "#define P(a,b) a##b\nP(,hello)";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"hello".to_string()),
            "expected 'hello' from paste with empty arg, got: {:?}",
            strs
        );
    }

    #[test]
    fn test_paste_start_of_body() {
        // ## at the start of a macro body is a constraint violation per
        // C99 6.10.3.3p1, but our preprocessor should handle it without
        // panicking. Just verify it completes.
        let code = "#define BAD(x) ##x\nBAD(hello)";
        let (_tokens, _idents) = preprocess_str(code);
        // If we get here without panicking, the test passes.
    }

    #[test]
    fn test_line_directive_sets_line() {
        // #line 100 should make __LINE__ report 100
        let (tokens, _idents) = preprocess_str("#line 100\n__LINE__");
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
        assert!(
            nums.contains(&"100".to_string()),
            "Expected __LINE__ to be 100, got {:?}",
            nums
        );
    }

    #[test]
    fn test_line_directive_sets_file() {
        // #line 200 "fake.c" should make __FILE__ report "fake.c"
        let (tokens, _idents) = preprocess_str("#line 200 \"fake.c\"\n__FILE__");
        let strs: Vec<_> = tokens
            .iter()
            .filter_map(|t| {
                if let TokenValue::String(s) = &t.value {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect();
        assert!(
            strs.contains(&"fake.c".to_string()),
            "Expected __FILE__ to be 'fake.c', got {:?}",
            strs
        );
    }

    #[test]
    fn test_line_directive_skipped_in_false_branch() {
        // #line inside #if 0 should have no effect
        let (tokens, _idents) = preprocess_str("#if 0\n#line 999 \"wrong.c\"\n#endif\n__LINE__");
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
        // __LINE__ should NOT be 999
        assert!(
            !nums.contains(&"999".to_string()),
            "__LINE__ should not be 999 in false branch"
        );
    }

    // ========================================================================
    // C99 compliance gap tests
    // ========================================================================

    #[test]
    fn test_line_directive_macro_expansion() {
        // #line should macro-expand its tokens before parsing
        let code = "#define LINENUM 100\n#line LINENUM\n__LINE__";
        let (tokens, _idents) = preprocess_str(code);
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
        assert!(
            nums.contains(&"100".to_string()),
            "Expected __LINE__ to be 100 after #line with macro, got {:?}",
            nums
        );
    }

    #[test]
    fn test_pragma_stdc_fp_contract() {
        // #pragma STDC FP_CONTRACT ON should be recognized without error
        let (tokens, idents) = preprocess_str("#pragma STDC FP_CONTRACT ON\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"code".to_string()),
            "code after #pragma STDC should pass through, got: {:?}",
            strs
        );
    }

    #[test]
    fn test_pragma_stdc_fenv_access() {
        let (tokens, idents) = preprocess_str("#pragma STDC FENV_ACCESS OFF\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_pragma_stdc_cx_limited_range() {
        let (tokens, idents) = preprocess_str("#pragma STDC CX_LIMITED_RANGE DEFAULT\ncode");
        let strs = get_token_strings(&tokens, &idents);
        assert!(strs.contains(&"code".to_string()));
    }

    #[test]
    fn test_stringify_string_literal() {
        // #x with x being "hello" should produce token with content \"hello\"
        // (C99 6.10.3.2p2: \ before each " and \ including string delimiters)
        let code = "#define S(x) #x\nS(\"hello\")";
        let (tokens, _idents) = preprocess_str(code);
        let strings: Vec<_> = tokens
            .iter()
            .filter_map(|t| {
                if let TokenValue::String(s) = &t.value {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect();
        // Token content should be: \"hello\" (escaped delimiters)
        assert!(
            strings.iter().any(|s| s == "\\\"hello\\\""),
            "expected stringified string with escaped delimiters, got: {:?}",
            strings
        );
    }

    #[test]
    fn test_stringify_char_literal() {
        // #x with x being 'a' should produce "'a'"
        let code = "#define S(x) #x\nS('a')";
        let (tokens, _idents) = preprocess_str(code);
        let strings: Vec<_> = tokens
            .iter()
            .filter_map(|t| {
                if let TokenValue::String(s) = &t.value {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect();
        assert!(
            strings.iter().any(|s| s.contains("'a'")),
            "expected stringified char literal, got: {:?}",
            strings
        );
    }

    #[test]
    fn test_pragma_operator_destringify() {
        // _Pragma with escaped content should not crash
        let code = "_Pragma(\"GCC diagnostic ignored \\\"warn\\\"\") int x;";
        let (tokens, idents) = preprocess_str(code);
        let strs = get_token_strings(&tokens, &idents);
        assert!(
            strs.contains(&"x".to_string()),
            "code after _Pragma should pass through, got: {:?}",
            strs
        );
    }
}
