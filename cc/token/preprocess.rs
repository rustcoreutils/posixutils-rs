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

use super::cursor::{Provenance, TokenCursor};
use super::lexer::{
    literal_payload, payload_bytes, payload_text, report_forbidden_ucn, show_token,
    tokens_to_source_bytes, write_token, IdentTable, LexerMode, Position, Punctuator, SpecialToken,
    Spelling, Token, TokenType, TokenValue, Tokenizer,
};
use super::literal;
use crate::arch;
use crate::builtin_headers;
use crate::diag;
use crate::os;
use crate::target::{Target, STDC_VERSION};
use gettextrs::gettext;

#[path = "preprocess_directive.rs"]
mod directive;

#[path = "preprocess_macro.rs"]
mod macros;

// `macro_redefinition_conflict` and the tests reach this by name.
use macros::replacement_lists_identical;

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

// Macro Definition

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
    /// How the token was written; see [`Token::spelling`]. Carried through a
    /// macro body so that `#define B u8"hi"` and `#define P %:%:` still spell
    /// themselves once expanded.
    pub spelling: Spelling,
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
    /// `__VA_ARGS__`, or the GNU name standing in for it. The variadic
    /// arguments begin at `params.len()`; nothing is carried here.
    VaArgs,
    /// The start of a `__VA_OPT__(...)` group.
    ///
    /// Flat markers rather than a nested list, because the substitution loop's
    /// paste lookbehind indexes `body[i-1]` and `body[i+1]` directly -- a
    /// nested group would put a list where a token has to be, and
    /// `x ## __VA_OPT__(y)` would stop pasting.
    VaOptStart {
        /// The index just past the matching [`MacroTokenValue::VaOptEnd`].
        end: usize,
        /// Whether a `#` was written against the group, making the result the
        /// spelling of what it produces rather than the tokens themselves.
        stringify: bool,
    },
    /// The end of a `__VA_OPT__(...)` group.
    VaOptEnd,
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

thread_local! {
    /// A string table for tokenizing predefined macro values.
    ///
    /// `Macro::predefined` needs a table only because the tokenizer takes one;
    /// every identifier it interns is de-interned again before the function
    /// returns, so no id escapes. Building a fresh table per call was not free:
    /// `StringTable::new` pre-interns every keyword at a fixed slot, and the
    /// compiler defines about fifty of these macros before it reads a line of
    /// source. That was half the instructions executed for an empty
    /// translation unit.
    ///
    /// Shared rather than keyword-free so the fixed keyword slots -- which the
    /// directive dispatch matches against as integers -- still hold.
    static SCRATCH_IDENTS: std::cell::RefCell<IdentTable> =
        std::cell::RefCell::new(IdentTable::new());
}

/// Run `f` with the shared scratch string table.
fn scratch_idents<T>(f: impl FnOnce(&mut IdentTable) -> T) -> T {
    SCRATCH_IDENTS.with(|cell| f(&mut cell.borrow_mut()))
}

impl Macro {
    /// Create a predefined macro (value is treated as a number/literal)
    pub fn predefined(name: &str, value: Option<&str>) -> Self {
        let body = match value {
            Some(v) => scratch_idents(|idents| {
                // Tokenize the value string properly so (-1021) becomes (, -, 1021, )
                let mut tokenizer = Tokenizer::new(v.as_bytes(), 0, idents);
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
                            // A header name cannot occur in a macro body --
                            // one is lexed only inside #include -- but the
                            // type carries it back either way.
                            TokenValue::HeaderName(h) => MacroTokenValue::String(h.clone()),
                            TokenValue::None => MacroTokenValue::None,
                        };
                        MacroToken {
                            typ: token.typ,
                            value,
                            whitespace: i > 0 && token.pos.whitespace,
                            spelling: token.spelling,
                        }
                    })
                    .collect()
            }),
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
                spelling: Spelling::Canonical,
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
                spelling: Spelling::Canonical,
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

// Conditional Compilation State

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
    ///
    /// Not quite what it says: `push_conditional` also sets it when the whole
    /// group is inside a skipped parent, so that no branch of a dead group can
    /// activate. It therefore cannot answer "was there an `#else`", which is
    /// why that is a separate flag.
    had_true: bool,
    /// Whether this group's `#else` has been seen.
    ///
    /// Without it a second `#else` is a legal state transition rather than an
    /// error, and a silently destructive one: it turns the group `Done`, so the
    /// first `#else` body is truncated and the second is dropped.
    seen_else: bool,
    /// Position of the #if/#ifdef/#ifndef directive
    pos: Position,
}

// Preprocessor

pub struct Preprocessor<'a> {
    /// Target configuration
    target: &'a Target,

    /// Macro definitions
    macros: HashMap<String, std::rc::Rc<Macro>>,

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

    /// How many `#include`s deep the pass currently is. This is what bounds a
    /// cycle; there is no set of open files.
    include_depth: u32,

    /// Maximum include depth
    max_include_depth: u32,

    /// Every header opened, in the order first opened, and whether it came
    /// from a system directory. Collected only when asked for (the `-M` family).
    ///
    /// Recorded where the file is *resolved* rather than where it is read, so a
    /// header that the `#pragma once` or include-guard fast path skips is still
    /// listed -- gcc lists it, and a makefile that omitted it would not rebuild
    /// when it changed.
    dependencies: Vec<(PathBuf, bool)>,
    /// Whether to collect the above.
    collect_dependencies: bool,

    /// Files named by a `#pragma once`.
    once_files: HashSet<PathBuf>,

    /// Files whose whole contents are one `#ifndef`/`#endif` group, and the
    /// macro that guards them.
    ///
    /// Only a file that has been read *through to the end* gets an entry, and
    /// only if its group closed with nothing outside it. That is what makes
    /// the entry mean "including this again would produce nothing", which is
    /// the only thing that justifies skipping it.
    ///
    /// Guessing instead -- scanning the text for an `#ifndef` and skipping
    /// whenever that name happened to be defined -- deleted source three ways:
    /// a header with anything after its `#endif` lost it on every include but
    /// the first; `-D<guard>` on the command line deleted the file entire,
    /// having never read it; and a header that includes itself under a counter
    /// guard lost its `#else` arm.
    ///
    /// Pure lookup, never iterated, so a `HashMap` cannot leak its order.
    guarded_files: HashMap<PathBuf, String>,

    /// Compilation date string for __DATE__ (format: "Mmm dd yyyy")
    compile_date: String,

    /// Compilation time string for __TIME__ (format: "hh:mm:ss")
    compile_time: String,

    /// Whether to use builtin headers (disabled by -nobuiltininc or -nostdinc)
    use_builtin_headers: bool,
    /// Apply translation phase 1 trigraph replacement to included files.
    trigraphs: bool,

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
    /// The one diagnostic stream every `##` result is attributed to.
    ///
    /// `diag::init_stream` appends to a registry and returns its length as a
    /// `u16`. Calling it per paste grew that registry without bound and wrapped
    /// silently past 65535, at which point *other* files' stream ids collided
    /// and every later diagnostic named the wrong file. A translation unit
    /// does far more than 65535 pastes.
    paste_stream: std::cell::Cell<Option<u16>>,
    /// How many more expansions may be spliced in before the pass gives up.
    /// Seeded once, at construction: the nested calls for an argument's own
    /// replacement and for an included file share it, since a runaway in
    /// either is the same failure, and re-seeding per call would let an
    /// exhausted budget come back.
    ///
    /// Termination rests entirely on the hide set now, and a hole in it is an
    /// infinite loop rather than a wrong answer -- a hang has no diagnostic and
    /// no exit status to read. This turns one into an error. It is a backstop,
    /// not a policy: the bound is far above what any real translation unit
    /// reaches, so hitting it means a bug here, not an unusual program.
    expansion_budget: u64,
    /// Set while a `#if`/`#elif` controlling expression is being expanded.
    ///
    /// C17 6.10.1p1 exempts the operand of `defined` from macro expansion, and
    /// the operand can only be recognised in the same walk that does the
    /// expanding: rewriting `defined X` in a pass *beforehand* misses a
    /// `defined` that an expansion produces, and expanding first destroys the
    /// operand. `#define D defined(FOO)` / `#if D` used to evaluate `0 (1)`.
    in_if_condition: bool,
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

/// The payload prefix a marker uses when it carries a pragma c17 does not act
/// on and only needs to reproduce.
const PRAGMA_TEXT_PREFIX: &str = "text:";

/// The directive a marker token stands for, when it is one c17 only carries.
///
/// `#pragma pack` is the one pragma that changes what the compiler does, so it
/// travels decoded, as a [`PackAction`]. Everything else travels as its own
/// text: c17 does not act on `#pragma GCC diagnostic` or an OpenMP directive,
/// but POSIX makes a `.i` a valid operand and c17 compiles one, so dropping
/// them made preprocessing and compiling in two steps mean something different
/// from doing it in one.
pub fn pragma_text(token: &Token) -> Option<String> {
    match &token.value {
        TokenValue::String(s) => s.strip_prefix(PRAGMA_TEXT_PREFIX).map(str::to_string),
        _ => None,
    }
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
            dependencies: Vec::new(),
            collect_dependencies: false,
            once_files: HashSet::with_capacity(DEFAULT_INCLUDE_TRACK_CAPACITY),
            guarded_files: HashMap::with_capacity(DEFAULT_INCLUDE_TRACK_CAPACITY),
            compile_date,
            compile_time,
            use_builtin_headers: true,
            trigraphs: false,
            current_include_path_index: None,
            lexer_mode: LexerMode::C,
            line_offset: 0,
            line_file_override: None,
            preprocessed: false,
            linemarker: None,
            physical_line: 0,
            physical_stream: 0,
            expansion_budget: EXPANSION_BUDGET,
            paste_stream: std::cell::Cell::new(None),
            in_if_condition: false,
        };

        // Include paths first: __STDC_NO_THREADS__ is decided by probing them.
        //
        // One list, in gcc's order: `-isystem` ahead of the target's own
        // directories, `-idirafter` behind them. Keeping it as a single vector
        // is what lets `#include_next`'s index walk stay coherent.
        for path in search.isystem {
            pp.system_include_paths.push(path.clone());
        }
        if !search.no_std_inc {
            for path in os::get_include_paths(target, search.sysroot) {
                pp.system_include_paths.push(path);
            }
        }
        for path in search.idirafter {
            pp.system_include_paths.push(path.clone());
        }

        pp.init_predefined_macros();

        pp
    }

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

    pub fn define_macro(&mut self, mac: Macro) {
        self.macros.insert(mac.name.clone(), std::rc::Rc::new(mac));
    }

    /// Process a `-include` file, as if the source began with `#include`.
    ///
    /// Named on the command line rather than written in the source, so there is
    /// no `#` to attribute it to; a `<command-line>` position stands in, the
    /// same one `-D` uses.
    pub fn include_from_cmdline(
        &mut self,
        path: &str,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
    ) {
        let stream_id = diag::init_stream("<command-line>");
        let hash = Token::new(TokenType::Special, Position::new(stream_id, 1, 1));

        // Searched like `#include "..."`: the working directory first, then
        // `-I`, then the system paths.
        match self.find_include_file(path, false, false) {
            Some((IncludeSource::File(found), index)) => {
                // A `-include` is a dependency exactly as a `#include` is.
                self.record_dependency(&found, index.is_some());
                self.include_file(&found, output, idents, &hash, index)
            }
            Some((IncludeSource::Builtin(content), _)) => {
                self.include_builtin(path, content, output, idents, &hash)
            }
            None => diag::error_args(hash.pos, "'{0}': file not found", &[path]),
        }
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
        //
        // The line flags are cleared too. A `-D` is one directive however the
        // shell wrapped it, but its first token is the first token of its own
        // buffer and so is flagged as beginning a line -- which the operand's
        // same-line check then reads as `#define` with nothing after it.
        // `-DGITVERSION="..."`, which CPython's build passes, was rejected.
        let mut cursor = TokenCursor::new(
            tokens
                .into_iter()
                .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
                .map(|mut t| {
                    t.pos.newline = false;
                    t
                })
                .collect(),
        );
        // A `-D` has no `#` to blame, so a malformed one is reported at the
        // start of the synthesized directive.
        let pos = cursor.peek().map(|t| t.pos).unwrap_or_default();
        self.handle_define(&mut cursor, idents, pos);
    }

    pub fn undef_macro(&mut self, name: &str) {
        self.macros.remove(name);
    }

    /// Check if a macro is defined
    pub fn is_defined(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// The `<paste>` stream id, created once.
    fn paste_stream(&self) -> u16 {
        match self.paste_stream.get() {
            Some(id) => id,
            None => {
                let id = diag::init_stream("<paste>");
                self.paste_stream.set(Some(id));
                id
            }
        }
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
        let mut output = Vec::new();
        // Tracks `defined X` / `defined ( X )` so the operand escapes macro
        // expansion; inert outside a controlling expression.
        let mut defined_scan = DefinedScan::Idle;
        let mut cursor = TokenCursor::new(tokens);

        while let Some(mut token) = cursor.next() {
            // Attribute the token before anything looks at it, so that macro
            // expansion, diagnostics and the `-E` marker writer all inherit
            // the position a linemarker established. The physical position is
            // kept because the next linemarker's delta is measured from it.
            //
            // Only for tokens read from the file. A token from an expansion
            // already carries the invocation's position, remapped when the
            // invoking token came through here, and `remap_pos` is not
            // idempotent when a linemarker's target is its own origin -- which
            // is what `# 100 "this-file.c"` produces -- so a second pass
            // applies the delta twice. Letting an expansion token write
            // `physical_line` is worse still: the next linemarker measures its
            // delta from that already-remapped line.
            if cursor.provenance() == Provenance::Main {
                self.physical_line = token.pos.line;
                self.physical_stream = token.pos.stream;
                token.pos = self.remap_pos(token.pos);
            }

            match token.typ {
                TokenType::StreamBegin | TokenType::StreamEnd => {
                    // Pass through stream markers
                    output.push(token);
                }

                TokenType::Special => {
                    if let TokenValue::Special(code) = &token.value {
                        // Check for # at start of line (preprocessor directive).
                        //
                        // Only from the file. C17 6.10.3p11 makes a directive
                        // produced by a macro expansion undefined, and taking
                        // one would be worse than undefined here: `skip_to_eol`
                        // and `collect_to_eol` stop at the next token that
                        // begins a line, so a stray `#` out of an expansion
                        // would swallow the rest of the file rather than the
                        // rest of a replacement list.
                        if *code == b'#' as u32
                            && token.pos.newline
                            && cursor.provenance() == Provenance::Main
                        {
                            self.handle_directive(&mut cursor, &token, &mut output, idents);
                            continue;
                        }
                    }
                    if !self.is_skipping() {
                        if self.in_if_condition {
                            defined_scan = defined_scan.punctuator(&token);
                        }
                        output.push(token);
                    }
                }

                TokenType::Ident => {
                    if self.is_skipping() {
                        continue;
                    }
                    // The operand of `defined` is not expanded (C17 6.10.1p1).
                    if self.in_if_condition {
                        let (next, protect) = defined_scan.identifier(&token, idents);
                        defined_scan = next;
                        if protect {
                            output.push(token);
                            continue;
                        }
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
                            // Everything that can reject this token is decided
                            // against the interned name, before it is copied.
                            // Almost every identifier in a translation unit is
                            // not a macro, and copying each one to find that
                            // out was an allocation per identifier -- the cost
                            // was paid by programs that define no macros at
                            // all. The copy is needed only past this point,
                            // because expanding wants `idents` mutably.
                            let is_pragma = name == "_Pragma";
                            let is_macro = self.macros.contains_key(name);
                            let hidden = token.is_no_expand(name);

                            // Handle _Pragma operator (C99)
                            // _Pragma("string") is equivalent to #pragma string
                            // We silently consume it since we ignore #pragma anyway
                            if is_pragma {
                                self.handle_pragma_operator(&mut cursor, &mut output);
                                continue;
                            }

                            // Per C99 6.10.3.4, a macro name that its own
                            // expansion put here is not expanded again.
                            if !is_macro || hidden {
                                output.push(token);
                                continue;
                            }
                            let name = name.to_string();

                            if let Some(expanded) =
                                self.try_expand_macro(&name, &token, &mut cursor, idents)
                            {
                                // Put the expansion back in front of the file
                                // rather than into the output, and do not
                                // advance. Rescanning is then this same loop
                                // reading on, and an expansion that ends in
                                // the middle of a call can finish it from the
                                // rest of the file -- which is what C17
                                // 6.10.3.4 EXAMPLE 3 asks for and what the
                                // recursive rescan could not do, because it
                                // only ever saw the replacement list.
                                //
                                // The invocation's spacing goes with it, to be
                                // applied to whichever token comes next: an
                                // expansion may be empty, or may begin with a
                                // macro that expands to nothing, and stamping
                                // the first token would lose it in both cases.
                                if !self.charge_expansion(token.pos) {
                                    output.push(token);
                                    continue;
                                }
                                cursor.push_expansion(
                                    expanded,
                                    token.pos.whitespace,
                                    token.pos.newline,
                                );
                                continue;
                            }
                        }
                    }
                    output.push(token);
                }

                _ => {
                    if !self.is_skipping() {
                        if self.in_if_condition {
                            defined_scan = DefinedScan::Idle;
                        }
                        output.push(token);
                    }
                }
            }
        }

        output
    }

    /// Account for one expansion, and say whether it may proceed.
    ///
    /// Reports once, then keeps returning false so the rest of the file is
    /// emitted without expansion rather than producing one error per token.
    fn charge_expansion(&mut self, pos: Position) -> bool {
        if self.expansion_budget == 0 {
            return false;
        }
        self.expansion_budget -= 1;
        if self.expansion_budget == 0 {
            diag::error(
                pos,
                &gettext("macro expansion is not terminating; giving up"),
            );
            return false;
        }
        true
    }

    /// Warn about `#if` groups the file never closed.
    ///
    /// This belongs to the whole pass, not to one call of it: `preprocess` is
    /// still re-entered for an argument's own macro replacement and for an
    /// included file, and only the outermost caller knows the run is over. It
    /// used to be guarded by a recursion depth counter for exactly that
    /// reason.
    ///
    /// An included file's leftover groups are not reported, because
    /// `include_file` swaps the conditional stack out around it and drops
    /// whatever is left. gcc does diagnose that; changing it is a separate
    /// question from this one.
    fn report_unterminated_conditionals(&mut self) {
        for cond in std::mem::take(&mut self.cond_stack) {
            diag::error(cond.pos, &gettext("unterminated #if"));
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

    /// Collect tokens until end of line
    fn collect_to_eol(&self, iter: &mut TokenCursor) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = iter.peek() {
            if token.pos.newline {
                break;
            }
            tokens.push(iter.next().unwrap());
        }
        tokens
    }

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
            seen_else: false,
            pos,
        });
    }

    /// Evaluate a controlling expression (C17 6.10.1).
    ///
    /// `directive_pos` is where to blame an expression that is missing
    /// altogether. A rejected expression is false, so the group is skipped and
    /// the error is what the user acts on; recovering to "whatever the parser
    /// happened to compute" is how a typo used to compile the wrong half of a
    /// file in silence.
    fn evaluate_expression(
        &self,
        tokens: &[Token],
        idents: &IdentTable,
        directive_pos: Position,
    ) -> bool {
        if tokens.is_empty() {
            diag::error(directive_pos, &gettext("#if with no expression"));
            return false;
        }
        let mut evaluator = ExprEvaluator::new(self, idents);
        let value = evaluator.evaluate(tokens);
        evaluator.check_fully_consumed();
        !evaluator.had_error && value.is_true()
    }

    fn token_to_string(&self, token: &Token, idents: &IdentTable) -> String {
        match &token.value {
            TokenValue::Ident(id) => idents.get_opt(*id).unwrap_or("").to_string(),
            TokenValue::Number(n) => n.clone(),
            TokenValue::String(s) => payload_text(s),
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

    /// Reduce `#pragma pack(...)`'s tokens to `PackTok`s and read them.
    fn parse_pack_pragma(
        &mut self,
        iter: &mut TokenCursor,
        idents: &IdentTable,
        pos: Position,
    ) -> Option<PackAction> {
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
}

// Expression Evaluator for #if

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
    /// Set once anything in the expression was rejected. The controlling
    /// expression then counts as false rather than as whatever the recovery
    /// path happened to compute: every `expr_*` used to fall through to zero
    /// in silence, so `#if (1`, `#if X = 2` and `#if 1.5` each picked a branch
    /// with no diagnostic at all.
    had_error: bool,
}

impl<'a, 'b> ExprEvaluator<'a, 'b> {
    fn new(pp: &'a Preprocessor<'b>, idents: &'a IdentTable) -> Self {
        Self {
            pp,
            idents,
            tokens: Vec::new(),
            pos: 0,
            suppressed: false,
            had_error: false,
        }
    }

    fn evaluate(&mut self, tokens: &[Token]) -> PpValue {
        self.tokens = tokens.to_vec();
        self.pos = 0;
        self.expr_ternary()
    }

    /// Reject the expression, unless this operand is being skipped by a
    /// short-circuit — `#if 0 && (1` must stay quiet for the same reason
    /// `#if 0 && 1/0` does.
    fn err(&mut self, pos: Position, msg: &str) {
        if self.suppressed {
            return;
        }
        self.had_error = true;
        diag::error(pos, msg);
    }

    /// Same, for a diagnostic that names the offending token.
    fn err_token(&mut self, pos: Position, template: &str, arg: &str) {
        if self.suppressed {
            return;
        }
        self.had_error = true;
        diag::error_args(pos, template, &[arg]);
    }

    /// C17 6.10.1p4: the line is *one* controlling expression. Anything left
    /// over is a typo the user wants to hear about — `#if 1 2 3` used to be
    /// simply true.
    fn check_fully_consumed(&mut self) {
        if self.had_error || self.pos >= self.tokens.len() {
            return;
        }
        let tok = self.tokens[self.pos].clone();
        let spelling = self.spell(&tok);
        // Two different mistakes, as gcc distinguishes them: an operand with
        // no operator joining it on (`#if 1 2`), versus a token that has no
        // meaning in a controlling expression at all (`#if X = 2`).
        if matches!(tok.typ, TokenType::Special) {
            self.err_token(
                tok.pos,
                "token \"{0}\" is not valid in preprocessor expressions",
                &spelling,
            );
        } else {
            self.err_token(
                tok.pos,
                "missing binary operator before token \"{0}\"",
                &spelling,
            );
        }
    }

    /// How a token should be named in a diagnostic.
    fn spell(&self, tok: &Token) -> String {
        self.pp.token_to_string(tok, self.idents)
    }

    /// The position to blame when the expression ran out of tokens.
    fn here(&self) -> Position {
        match self.current() {
            Some(tok) => tok.pos,
            None => self.tokens.last().map(|t| t.pos).unwrap_or_default(),
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
            let op_pos = self.here();
            self.advance();
            let right = self.expr_additive();
            // A shift does not apply the usual arithmetic conversions: the
            // result takes the left operand's type (C17 6.5.7p3). A count
            // outside [0, 64) is undefined; clamp rather than panic, but say
            // so -- clamping in silence made `#if (1 << 64) == 0` false with
            // nothing to explain it. gcc warns here rather than erroring, so
            // the expression still evaluates.
            if !self.suppressed && !(0..64).contains(&right.v) {
                diag::warning(
                    op_pos,
                    &gettext("integer overflow in preprocessor expression"),
                );
            }
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
            } else {
                let pos = self.here();
                self.err(pos, &gettext("missing ')' in expression"));
            }
            return val;
        }

        // Handle number
        if let Some(tok) = self.current() {
            if let TokenValue::Number(n) = &tok.value {
                let num_str = n.clone();
                let pos = tok.pos;
                self.advance();
                return self.parse_number(&num_str, pos);
            }
        }

        // Handle character literal (any encoding prefix: L'x', u'x', U'x')
        if let Some(tok) = self.current() {
            let wide = matches!(
                &tok.value,
                TokenValue::WideChar(_) | TokenValue::Utf16Char(_) | TokenValue::Utf32Char(_)
            );
            let char_str = match &tok.value {
                TokenValue::Char(c)
                | TokenValue::WideChar(c)
                | TokenValue::Utf16Char(c)
                | TokenValue::Utf32Char(c) => Some(c.clone()),
                _ => None,
            };
            if let Some(char_str) = char_str {
                let pos = tok.pos;
                self.advance();
                return self.char_constant(&char_str, wide, pos);
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

        // Nothing here can start an operand. Reaching this used to return zero
        // in silence, which is why a string literal, a stray punctuator or a
        // missing operand all quietly chose a branch.
        match self.current().cloned() {
            None => {
                let pos = self.here();
                self.err(pos, &gettext("expression expected"));
            }
            Some(tok) => {
                let spelling = self.spell(&tok);
                // Do not advance: the caller's operator loop stops on this
                // token, and consuming it here would hide the rest of the line.
                self.err_token(
                    tok.pos,
                    "token \"{0}\" is not valid in preprocessor expressions",
                    &spelling,
                );
            }
        }
        PpValue::signed(0)
    }

    /// The value of a character constant in a controlling expression.
    ///
    /// The token payload is the source spelling between the quotes, so the
    /// escapes are still in it: `'\n'` arrives as the two characters `\` and
    /// `n`. Packing that verbatim is how `#if '\n'` came to be 23662 and
    /// `#if '\0'` came to be *true*. Decoding goes through the same
    /// [`literal`] module the parser uses, so `#if 'c' == V` and the compiled
    /// `'c' == V` cannot disagree.
    fn char_constant(&mut self, payload: &str, wide: bool, pos: Position) -> PpValue {
        let elements = literal::parse_string_literal(payload);
        for e in &elements {
            if let literal::Escaped::ForbiddenUcn(val) = e {
                if !self.suppressed {
                    report_forbidden_ucn(pos, *val);
                }
            }
        }
        if elements.is_empty() {
            self.err(pos, "empty character constant");
            return PpValue::signed(0);
        }

        // A prefixed constant holds characters, not bytes: `L'\n'` is the one
        // wide character 10, never the two bytes of a UTF-8 encoding.
        if wide {
            let units = literal::literal_wide_chars(&elements);
            return PpValue::signed(units.first().copied().unwrap_or(0) as i128);
        }

        // C17 6.4.4.4p10: an ordinary character constant has type `int`. One
        // character takes plain `char`'s signedness, so `'\xff'` is negative
        // where `char` is signed and positive where it is not -- which is the
        // whole reason `Target::char_signed` exists.
        let bytes: Vec<u8> = payload_bytes(&literal::literal_bytes(&elements)).collect();
        if bytes.len() == 1 {
            let b = bytes[0];
            return PpValue::signed(if self.pp.target.char_signed {
                b as i8 as i128
            } else {
                b as i128
            });
        }

        // More than one: gcc packs big-endian and lets the value wrap in
        // `int`, so `'abcde'` keeps only its last four bytes.
        let mut val: u32 = 0;
        for b in bytes {
            val = (val << 8) | b as u32;
        }
        PpValue::signed(val as i32 as i128)
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
    fn parse_number(&mut self, s: &str, pos: Position) -> PpValue {
        // C17 6.10.1p4: the operands are integer constants. A pp-number that
        // is not one used to evaluate to zero in silence, so `#if 1.5` picked
        // the else branch and `#if 1zz` picked it too.
        if s.contains('.')
            || ((s.contains('e') || s.contains('E'))
                && !s.starts_with("0x")
                && !s.starts_with("0X"))
            || ((s.contains('p') || s.contains('P'))
                && (s.starts_with("0x") || s.starts_with("0X")))
        {
            self.err_token(
                pos,
                "floating constant \"{0}\" in preprocessor expression",
                s,
            );
            return PpValue::signed(0);
        }

        // Split at the first character the radix cannot spell, rather than
        // trimming a suffix off the end: `1zz` has no valid suffix to trim, so
        // the old trim left `1zz` as the body and the parse failure went
        // unreported.
        let (prefix, radix) =
            if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
                (hex, 16)
            } else if let Some(bin) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
                (bin, 2)
            } else if s.len() > 1
                && s.starts_with('0')
                && s[1..].starts_with(|c: char| c.is_ascii_digit())
            {
                (&s[1..], 8)
            } else {
                (s, 10)
            };
        let digit_len = prefix
            .find(|c: char| !c.is_digit(radix))
            .unwrap_or(prefix.len());
        let (body, suffix) = prefix.split_at(digit_len);

        if body.is_empty() {
            self.err_token(pos, "invalid integer constant \"{0}\"", s);
            return PpValue::signed(0);
        }
        if !suffix_is_valid(suffix) {
            self.err_token(pos, "invalid suffix \"{0}\" on integer constant", suffix);
            return PpValue::signed(0);
        }
        let suffix_unsigned = suffix.contains('u') || suffix.contains('U');

        match u64::from_str_radix(body, radix) {
            Ok(v) => {
                // Too large for intmax_t means the constant's type is
                // uintmax_t, even without a suffix.
                let unsigned = suffix_unsigned || v > i64::MAX as u64;
                PpValue::from_parts(v as i128, unsigned)
            }
            // The body is all digits of the radix, so the only way to fail is
            // to be wider than `uintmax_t`.
            Err(_) => {
                self.err_token(pos, "integer constant \"{0}\" is too large", s);
                PpValue::signed(0)
            }
        }
    }
}

/// The macro-expansion backstop. Orders of magnitude above what a real
/// translation unit reaches -- CPython's largest needs a few hundred thousand
/// -- so exhausting it means a hole in the hide set, not an unusual program.
const EXPANSION_BUDGET: u64 = 1 << 26;

/// Where a `#if` scan is within a `defined` operator.
///
/// The operand must not be macro-expanded (C17 6.10.1p1), and recognising it
/// requires walking the condition in the same pass that expands the rest: a
/// pass beforehand cannot see a `defined` that an expansion produces, and a
/// pass afterwards finds the operand already expanded away. This is the four
/// states that walk needs -- the same shape as sparse's `expression_value`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum DefinedScan {
    /// Not in a `defined` operator.
    Idle,
    /// Just past `defined`; the next token is either `(` or the operand.
    SawOperator,
    /// Just past `defined (`; the next identifier is the operand.
    SawParen,
    /// Past `defined ( X`; expecting the `)`.
    NeedClose,
}

impl DefinedScan {
    /// Step over an identifier. The `bool` says whether this identifier is a
    /// `defined` operand and must therefore be emitted unexpanded.
    fn identifier(self, token: &Token, idents: &IdentTable) -> (Self, bool) {
        let is_operator = match &token.value {
            TokenValue::Ident(id) => idents.get_opt(*id) == Some("defined"),
            _ => false,
        };
        match self {
            // `defined defined` -- the operand is the identifier, whatever it
            // is spelled, so it is protected rather than treated as a second
            // operator.
            Self::SawOperator | Self::SawParen => {
                let next = if self == Self::SawParen {
                    Self::NeedClose
                } else {
                    Self::Idle
                };
                (next, true)
            }
            _ if is_operator => (Self::SawOperator, false),
            _ => (Self::Idle, false),
        }
    }

    /// Step over a punctuator.
    fn punctuator(self, token: &Token) -> Self {
        let TokenValue::Special(code) = &token.value else {
            return Self::Idle;
        };
        match self {
            Self::SawOperator if *code == b'(' as u32 => Self::SawParen,
            Self::NeedClose if *code == b')' as u32 => Self::Idle,
            _ => Self::Idle,
        }
    }
}

/// A synthetic decimal pp-number token, for the `0`/`1` a `defined` operator
/// stands for and the `0` an unresolved identifier becomes.
fn pp_number(text: &str, pos: Position) -> Token {
    Token {
        typ: TokenType::Number,
        value: TokenValue::Number(text.to_string()),
        pos,
        spelling: Spelling::Canonical,
        no_expand: None,
    }
}

/// The integer-suffix grammar of C17 6.4.4.1: `u`/`U` at most once, and `l`,
/// `L`, `ll` or `LL` at most once, in either order.
fn suffix_is_valid(suffix: &str) -> bool {
    let mut rest = suffix;
    let mut seen_unsigned = false;
    let mut seen_long = false;
    while !rest.is_empty() {
        if !seen_unsigned && (rest.starts_with('u') || rest.starts_with('U')) {
            seen_unsigned = true;
            rest = &rest[1..];
        } else if !seen_long && (rest.starts_with("ll") || rest.starts_with("LL")) {
            seen_long = true;
            rest = &rest[2..];
        } else if !seen_long && (rest.starts_with('l') || rest.starts_with('L')) {
            seen_long = true;
            rest = &rest[1..];
        } else {
            return false;
        }
    }
    true
}

// Public API

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
    /// `-nostdinc`: leave the *target's own* directories out of the search.
    ///
    /// Only those. `-isystem` and `-idirafter` name directories the caller
    /// asked for explicitly and are still searched, which is what gcc does --
    /// `gcc -nostdinc -isystem d` finds `<h.h>` in `d`. Treating `-nostdinc`
    /// as "no system paths at all" dropped those too.
    pub no_std_inc: bool,
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
    /// Files to process as if each were a `#include "..."` on the line before
    /// the source, in the order given (`-include`).
    pub pre_includes: &'a [String],
    /// Collect every macro definition for `-dM` instead of only the tokens.
    pub dump_macros: bool,
    /// Collect the headers this translation unit depends on (the `-M` family).
    pub collect_dependencies: bool,
    /// What optimization was asked for.
    ///
    /// The same value the optimizer is given, so `__OPTIMIZE__`,
    /// `__OPTIMIZE_SIZE__` and `__NO_INLINE__` are derived from what the
    /// compiler will actually do rather than from a copy of it. A capability
    /// macro that can disagree with the capability is the failure this
    /// arrangement exists to prevent.
    pub optimization: crate::opt::Optimization,
}

/// Define the macros that say what optimization is being done.
///
/// GCC and Clang agree on all three: `__OPTIMIZE__` whenever the level is not
/// zero, `__OPTIMIZE_SIZE__` when optimizing for size, and `__NO_INLINE__`
/// when functions are not inlined on their merits -- at `-O0`, or under
/// `-fno-inline` at any level.
///
/// `__NO_INLINE__` does not mean `always_inline` stops working. GCC defines it
/// and still honours the attribute, and glibc's `__fortify_function` depends
/// on exactly that combination.
///
/// Defined before `-D` and `-U` are applied, so a user can still override any
/// of them, which is also what GCC allows.
fn define_optimization_macros(pp: &mut Preprocessor, opt: crate::opt::Optimization) {
    if opt.optimizes() {
        pp.define_macro(Macro::predefined("__OPTIMIZE__", Some("1")));
    }
    if opt.for_size() {
        pp.define_macro(Macro::predefined("__OPTIMIZE_SIZE__", Some("1")));
    }
    if !opt.inlines_generally() {
        pp.define_macro(Macro::predefined("__NO_INLINE__", Some("1")));
    }
}

/// What preprocessing found, beyond the tokens.
///
/// The `Preprocessor` is a local of the function below and is dropped when it
/// returns, so anything a caller wants to *collect* has to leave through here.
#[derive(Debug, Default)]
pub struct PreprocessOutcome {
    /// Every macro in force at the end, as `#define` lines, sorted (`-dM`).
    /// Empty unless asked for: rendering them is not free.
    pub macro_definitions: Vec<String>,
    /// Every header opened, in the order first opened, with whether it came
    /// from a system directory (the `-M` family). Empty unless asked for.
    pub dependencies: Vec<(PathBuf, bool)>,
}

/// Preprocess tokens with command-line defines and undefines, collecting
/// whatever [`PreprocessConfig`] asked to be collected.
///
/// This is the entry point for preprocessing: lexer output in, preprocessed
/// tokens out. A caller that wants only the tokens ignores the outcome.
pub fn preprocess_collecting(
    tokens: Vec<Token>,
    target: &Target,
    idents: &mut IdentTable,
    filename: &str,
    config: &PreprocessConfig<'_>,
) -> (Vec<Token>, PreprocessOutcome) {
    let mut pp = Preprocessor::new(target, filename, &config.search);

    // -nostdinc drops the bundled headers as well as the target's own
    // directories, which is what gcc does: `gcc -nostdinc` cannot find
    // <stddef.h> either, its own include directory being one of the standard
    // ones. Probed rather than assumed -- this was very nearly "aligned" the
    // other way. The directories themselves are left out in
    // `Preprocessor::new`, which is the only place that knows which of them
    // came from `-isystem`.
    if config.no_std_inc {
        pp.use_builtin_headers = false;
    }
    if config.no_builtin_inc {
        pp.use_builtin_headers = false;
    }
    pp.trigraphs = config.trigraphs;
    pp.preprocessed = config.preprocessed;
    pp.collect_dependencies = config.collect_dependencies;

    define_optimization_macros(&mut pp, config.optimization);

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

    // `-include` runs after `-D`/`-U`, because a header may well test what
    // they defined, and before the source, because that is what "as if it were
    // the first line" means.
    let mut included = Vec::new();
    for path in config.pre_includes {
        pp.include_from_cmdline(path, &mut included, idents);
    }

    let mut output = pp.preprocess(tokens, idents);
    pp.report_unterminated_conditionals();

    if !included.is_empty() {
        // After the source's own `StreamBegin`, not before it. The marker says
        // where the translation unit starts, and everything downstream reads
        // the stream structure -- a `StreamBegin` arriving partway through the
        // token vector is not something any consumer expects.
        let at = usize::from(matches!(
            output.first().map(|t| t.typ),
            Some(TokenType::StreamBegin)
        ));
        output.splice(at..at, included);
    }

    let outcome = PreprocessOutcome {
        macro_definitions: if config.dump_macros {
            pp.macro_definitions(idents)
        } else {
            Vec::new()
        },
        dependencies: std::mem::take(&mut pp.dependencies),
    };
    (output, outcome)
}

// Assembly File Preprocessing

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
    /// What optimization was asked for; see [`PreprocessConfig::optimization`].
    /// GCC defines these for `.S` files too.
    pub optimization: crate::opt::Optimization,
}

/// Preprocess an assembly file (.S) and return the preprocessed text.
///
/// This uses the same preprocessor as C files but with assembly-specific
/// comment syntax (`;` for line comments, no `//` or `/* */`).
///
/// # Returns
/// The preprocessed assembly text, as bytes: a string literal's payload is a
/// byte sequence, so rendering it through a Rust `String` would re-encode
/// every byte >= 0x80.
pub fn preprocess_asm_file(
    content: &[u8],
    target: &Target,
    filename: &str,
    config: &AsmPreprocessConfig<'_>,
) -> Vec<u8> {
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

    define_optimization_macros(&mut pp, config.optimization);

    // -nostdinc: the directories are dropped in `Preprocessor::new`; the
    // bundled headers go with them.
    if config.no_std_inc {
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
    pp.report_unterminated_conditionals();

    // Convert tokens back to text
    tokens_to_source_bytes(&preprocessed, &strings)
}

#[cfg(test)]
#[path = "test_preprocess.rs"]
mod tests;
