//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Macro definition, substitution and expansion: turning a `#define` body into
// a replacement list, and a use of it back into tokens.
//
// A child module of `preprocess`, so these still reach `Preprocessor`'s
// private fields and the parent's helpers exactly as they did inline.
// `pub(super)` marks only what the parent, a sibling, or the tests call.
//

use super::*;

/// Compare two replacement lists, ignoring whitespace before the first token.
pub(super) fn replacement_lists_identical(a: &[MacroToken], b: &[MacroToken]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b).enumerate().all(|(i, (x, y))| {
        x.typ == y.typ && x.value == y.value && (i == 0 || x.whitespace == y.whitespace)
    })
}

/// Strip the "begins a line" flag from a token being placed into a macro
/// expansion.
///
/// Nothing an expansion produces starts a line: C17 6.10.3p11 makes a
/// directive built by an expansion undefined, and `-E` breaks lines on this
/// flag. Argument tokens are the ones that carry it in, since they come
/// straight from the file and an argument list may span lines.
fn clear_newline(mut token: Token) -> Token {
    token.pos.newline = false;
    token
}

/// Hide, on every token an expansion produced, each name the invocation was
/// already hiding plus the macro's own (C17 6.10.3.4p2).
///
/// Two things about this are load-bearing.
///
/// It is applied to the *final* list, after pasting. `paste_tokens` re-lexes
/// its operands and returns fresh tokens with no hide set at all, which is
/// right -- a pasted token is a new token and is eligible for expansion -- but
/// it means a macro name that `##` synthesises would otherwise escape with
/// nothing hiding it. `#define X A##B` with `#define AB X` then never
/// terminates.
///
/// Argument-derived tokens are included, never exempt. `#define f(x) x(x)`
/// with `f(f)` substitutes `f` into a body that immediately calls it; if the
/// substituted `f` kept only its own hide set, each round would produce
/// another unhidden `f` and the expansion would not terminate either.
///
/// The whole set rides on each identifier token, not just names matching that
/// token's own spelling. Recording only self-names looks equivalent -- a hide
/// set is read in exactly one place, when that token is the identifier about
/// to be expanded -- but it breaks the moment a chain runs through a *different*
/// name. `#define __inline inline` with `#define inline __inline`, which is
/// expat's portability shim, expands `__inline` to an `inline` that hides
/// nothing, which expands back to an unhidden `__inline`, forever. The set has
/// to reach the token that will carry it into the next expansion.
///
/// Only the tokens whose set can ever be read carry one: identifiers, which
/// may be the next thing expanded, and closing parentheses, which decide half
/// of a function-like invocation's hide set.
fn hide_in_expansion(
    tokens: &mut [Token],
    macro_name: &str,
    invoker_hide: Option<&std::collections::HashSet<String>>,
) {
    for tok in tokens.iter_mut() {
        // Identifiers, because a hide set is read when the token is the one
        // about to be expanded -- and closing parentheses, because a
        // function-like invocation's result hides what *both* ends of the call
        // were hiding, so `)` is the one punctuator whose set is ever read.
        // Everything else would be bytes nothing can look at.
        let readable = matches!(tok.value, TokenValue::Ident(_))
            || matches!(&tok.value, TokenValue::Special(c) if *c == b')' as u32);
        if !readable {
            continue;
        }
        if let Some(hide) = invoker_hide {
            for name in hide {
                tok.mark_no_expand(name);
            }
        }
        tok.mark_no_expand(macro_name);
    }
}

/// A function-like macro's arguments, and what its closing `)` was hiding.
///
/// The second half decides half of the invocation's own hide set (C17
/// 6.10.3.4), so it has to travel with the arguments rather than be recovered
/// afterwards -- by then the `)` has been consumed.
type MacroCall = (Vec<Vec<Token>>, Option<HashSet<String>>);

impl<'a> Preprocessor<'a> {
    /// Resolve one `defined` operator: `defined X` or `defined ( X )`.
    ///
    /// `i` indexes the token just after `defined`. Returns the `0`/`1` it
    /// stands for and the index past the operand.
    fn resolve_defined(
        &self,
        tokens: &[Token],
        mut i: usize,
        pos: Position,
        idents: &IdentTable,
    ) -> (Token, usize) {
        let has_paren = matches!(
            tokens.get(i).map(|t| &t.value),
            Some(TokenValue::Special(code)) if *code == b'(' as u32
        );
        if has_paren {
            i += 1;
        }

        let is_defined = match tokens.get(i).map(|t| &t.value) {
            Some(TokenValue::Ident(id)) => {
                let defined = idents.get_opt(*id).is_some_and(|n| self.is_defined(n));
                i += 1;
                defined
            }
            _ => {
                diag::error(pos, &gettext("operator \"defined\" requires an identifier"));
                false
            }
        };

        if has_paren {
            let closed = matches!(
                tokens.get(i).map(|t| &t.value),
                Some(TokenValue::Special(code)) if *code == b')' as u32
            );
            if closed {
                i += 1;
            } else {
                diag::error(pos, &gettext("missing ')' after \"defined\""));
            }
        }

        (pp_number(if is_defined { "1" } else { "0" }, pos), i)
    }

    /// Expand macros in `#if`/`#elif` condition tokens.
    ///
    /// C17 6.10.1p1: macros are expanded, except for the operand of `defined`,
    /// and every identifier that survives becomes `0`.
    ///
    /// `defined` is recognised on both sides of expansion. Doing it only
    /// beforehand -- which is what the standard's wording literally describes
    /// -- left a `defined` that came *out* of an expansion to be zeroed along
    /// with everything else, so `#define D defined(FOO)` / `#if D` evaluated
    /// `0 (1)`. That form is undefined behaviour, but gcc, clang and MSVC all
    /// agree on the answer and the `IS_ENABLED(x)` idiom depends on it.
    pub(super) fn expand_if_tokens(
        &mut self,
        tokens: &[Token],
        idents: &mut IdentTable,
    ) -> Vec<Token> {
        // Expand, with `defined` operands protected on the way through. The
        // condition decides whether to skip, so the expansion itself must not
        // be skipped: push a dummy active group.
        self.cond_stack.push(Conditional {
            state: CondState::Active,
            had_true: true,
            seen_else: false,
            pos: Position::default(),
        });
        let saved_in_if = std::mem::replace(&mut self.in_if_condition, true);
        let expanded = self.preprocess(tokens.to_vec(), idents);
        self.in_if_condition = saved_in_if;
        self.cond_stack.pop();

        // Now resolve the operators, then zero whatever identifiers remain.
        let mut final_result = Vec::new();
        let mut i = 0;
        while i < expanded.len() {
            let token = &expanded[i];
            let TokenValue::Ident(id) = &token.value else {
                final_result.push(token.clone());
                i += 1;
                continue;
            };
            match idents.get_opt(*id) {
                Some("defined") => {
                    let (tok, next) = self.resolve_defined(&expanded, i + 1, token.pos, idents);
                    final_result.push(tok);
                    i = next;
                }
                // Still a macro name after expansion means it was blue-painted
                // by its own expansion; it is not defined *here*, so it is 0
                // like any other identifier.
                _ => {
                    final_result.push(pp_number("0", token.pos));
                    i += 1;
                }
            }
        }

        final_result
    }

    /// Convert tokens to macro body
    /// The index just past the `)` closing a `__VA_OPT__` whose identifier is
    /// at `i`, or `None` if it is not followed by a balanced `( ... )`.
    fn va_opt_extent(tokens: &[Token], i: usize) -> Option<usize> {
        let is_punct =
            |t: &Token, c: u8| matches!(&t.value, TokenValue::Special(code) if *code == c as u32);
        if !tokens.get(i + 1).is_some_and(|t| is_punct(t, b'(')) {
            return None;
        }
        // Balanced, because the content may itself contain parentheses --
        // `__VA_OPT__(f(a,b))` is one group, not a group ending at the inner
        // `)`.
        let mut depth = 0usize;
        for (j, t) in tokens.iter().enumerate().skip(i + 1) {
            if is_punct(t, b'(') {
                depth += 1;
            } else if is_punct(t, b')') {
                depth -= 1;
                if depth == 0 {
                    return Some(j + 1);
                }
            }
        }
        None
    }

    /// Where the comma is, if what has been built so far ends in the GNU
    /// `, ##` that precedes a `__VA_ARGS__`.
    ///
    /// Returns the index of the comma, so the caller can drop both it and the
    /// `##` and put a `__VA_OPT__(,)` group there instead.
    fn gnu_comma_elision(body: &[MacroToken]) -> Option<usize> {
        let n = body.len();
        if n < 2 || !matches!(body[n - 1].value, MacroTokenValue::Paste) {
            return None;
        }
        match body[n - 2].value {
            MacroTokenValue::Special(c) if c == b',' as u32 => Some(n - 2),
            _ => None,
        }
    }

    /// Build the flat marker group for a `__VA_OPT__(...)` whose identifier is
    /// at `at`, returning the tokens and the index just past the group.
    ///
    /// `end` inside the `VaOptStart` is relative to the group, and is fixed up
    /// by [`Self::patch_va_opt_end`] once the caller knows where the group
    /// landed in the body.
    #[allow(clippy::too_many_arguments)]
    fn build_va_opt(
        &self,
        tokens: &[Token],
        at: usize,
        stringify: bool,
        whitespace: bool,
        params: &[MacroParam],
        variadic_name: Option<&str>,
        is_function: bool,
        idents: &IdentTable,
    ) -> Option<(Vec<MacroToken>, usize)> {
        let after_close = Self::va_opt_extent(tokens, at)?;
        // The content goes through the same analysis, so `#param`, `##` and
        // `__VA_ARGS__` inside the group mean what they mean anywhere else.
        let inner = self.tokens_to_macro_body(
            &tokens[at + 2..after_close - 1],
            params,
            variadic_name,
            is_function,
            true,
            idents,
        );
        let mut group = Vec::with_capacity(inner.len() + 2);
        group.push(MacroToken {
            typ: TokenType::Ident,
            value: MacroTokenValue::VaOptStart { end: 0, stringify },
            whitespace,
            spelling: Spelling::Canonical,
        });
        group.extend(inner);
        group.push(MacroToken {
            typ: TokenType::Ident,
            value: MacroTokenValue::VaOptEnd,
            whitespace: false,
            spelling: Spelling::Canonical,
        });
        Some((group, after_close))
    }

    /// Fill in the `end` of the group that was just pushed onto `body`.
    fn patch_va_opt_end(body: &mut [MacroToken]) {
        let end = body.len();
        // The group is the tail; its start is the last unfilled marker.
        for i in (0..body.len()).rev() {
            if let MacroTokenValue::VaOptStart { end: slot, .. } = &mut body[i].value {
                if *slot == 0 {
                    *slot = end;
                    return;
                }
            }
        }
    }

    pub(super) fn tokens_to_macro_body(
        &self,
        tokens: &[Token],
        params: &[MacroParam],
        variadic_name: Option<&str>,
        is_function: bool,
        is_variadic: bool,
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
                                    spelling: Spelling::Canonical,
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
                                            spelling: Spelling::Canonical,
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
                                        spelling: Spelling::Canonical,
                                    });
                                    i += 2;
                                    continue;
                                }
                                // `#__VA_OPT__(...)`: the group's spelling, or
                                // `""` when there are no variadic arguments.
                                // The group builder is shared with the
                                // unprefixed form, one token further on.
                                if name == "__VA_OPT__" && is_variadic {
                                    if let Some(built) = self.build_va_opt(
                                        tokens,
                                        i + 1,
                                        true,
                                        token.pos.whitespace,
                                        params,
                                        variadic_name,
                                        is_function,
                                        idents,
                                    ) {
                                        let (group, after) = built;
                                        body.extend(group);
                                        Self::patch_va_opt_end(&mut body);
                                        i = after;
                                        continue;
                                    }
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
                        spelling: Spelling::Canonical,
                    });
                    i += 1;
                    continue;
                }
            }

            // Check for parameter reference or __VA_ARGS__
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    // `__VA_OPT__(content)`: content when the variadic
                    // arguments are non-empty, nothing when they are not
                    // (C23 6.10.5.1).
                    if name == "__VA_OPT__" {
                        match Self::va_opt_extent(tokens, i) {
                            Some(_) if is_variadic => {
                                let (group, after) = self
                                    .build_va_opt(
                                        tokens,
                                        i,
                                        false,
                                        token.pos.whitespace,
                                        params,
                                        variadic_name,
                                        is_function,
                                        idents,
                                    )
                                    .expect("extent already checked");
                                body.extend(group);
                                Self::patch_va_opt_end(&mut body);
                                i = after;
                                continue;
                            }
                            Some(_) => {
                                // gcc warns and leaves it alone rather than
                                // rejecting the definition.
                                diag::warning(
                                    token.pos,
                                    &gettext("__VA_OPT__ is only meaningful in a variadic macro"),
                                );
                            }
                            None => {
                                diag::error(token.pos, &gettext("expected '(' after __VA_OPT__"));
                            }
                        }
                    }

                    // Check if it's __VA_ARGS__, or the GNU named variadic
                    // that stands in for it.
                    if name == "__VA_ARGS__" || Some(name) == variadic_name {
                        // The GNU `, ## __VA_ARGS__` extension means exactly
                        // `__VA_OPT__(,) __VA_ARGS__`: keep the comma when
                        // there are trailing arguments, drop it when there are
                        // not. Lower it here so there is one mechanism rather
                        // than a second, subtly different one in the
                        // substitution loop -- the old special case fired on
                        // "the previous body token was `##`" alone and, when
                        // the arguments were non-empty, never actually pasted.
                        if is_variadic {
                            if let Some(comma) = Self::gnu_comma_elision(&body) {
                                body.truncate(comma);
                                body.push(MacroToken {
                                    typ: TokenType::Ident,
                                    value: MacroTokenValue::VaOptStart {
                                        end: comma + 3,
                                        stringify: false,
                                    },
                                    whitespace: true,
                                    spelling: Spelling::Canonical,
                                });
                                body.push(MacroToken {
                                    typ: TokenType::Special,
                                    value: MacroTokenValue::Special(b',' as u32),
                                    whitespace: false,
                                    spelling: Spelling::Canonical,
                                });
                                body.push(MacroToken {
                                    typ: TokenType::Ident,
                                    value: MacroTokenValue::VaOptEnd,
                                    whitespace: false,
                                    spelling: Spelling::Canonical,
                                });
                            }
                        }
                        body.push(MacroToken {
                            typ: TokenType::Ident,
                            value: MacroTokenValue::VaArgs,
                            whitespace: token.pos.whitespace,
                            spelling: Spelling::Canonical,
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
                                spelling: Spelling::Canonical,
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
            TokenValue::HeaderName(h) => MacroTokenValue::String(h.clone()),
            TokenValue::None => MacroTokenValue::None,
        };

        MacroToken {
            typ: token.typ,
            value,
            whitespace: token.pos.whitespace,
            spelling: token.spelling,
        }
    }

    /// Render tokens as the message of a `#error` or `#warning`, spelled the
    /// way they were written.
    ///
    /// Not to be confused with the free `tokens_to_source_bytes`, which
    /// reproduces a whole translation unit for `-E` with its line structure
    /// intact. This one produces one line for a human to read.
    ///
    /// Built on `show_token` so that every token type is covered: the
    /// hand-written match this replaces handled four of them and silently
    /// dropped the rest, so `#error "a" 'b' L"c"` reported `#error a  and`
    /// -- quotes stripped, two of the three operands gone.
    pub(super) fn tokens_to_message(&self, tokens: &[Token], idents: &IdentTable) -> String {
        let mut bytes: Vec<u8> = Vec::new();
        for token in tokens {
            if !bytes.is_empty() && token.pos.whitespace {
                bytes.push(b' ');
            }
            write_token(&mut bytes, token, idents);
        }
        // Decoded once at the end, not per token: a multi-byte character
        // outside a literal is several single-byte punctuators, and neither
        // half is valid UTF-8 alone. Decoding each separately turned `café`
        // in a `#error` into two replacement characters.
        String::from_utf8_lossy(&bytes).into_owned()
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
                        // An identifier is Rust text -- a UCN in it decoded to
                        // a real `char` -- while the result is a literal
                        // payload, one `char` per byte.
                        result.push_str(&literal_payload(name));
                    }
                }
                TokenValue::Number(n) => result.push_str(n),
                TokenValue::String(s)
                | TokenValue::WideString(s)
                | TokenValue::Utf16String(s)
                | TokenValue::Utf32String(s) => {
                    // The encoding prefix is part of the spelling, so it must
                    // survive stringification (C99 6.10.3.2p2). `u8` is not
                    // one of the token types -- it folds into the narrow one
                    // -- so it comes off the token's own flag.
                    result.push_str(match &token.value {
                        TokenValue::WideString(_) => "L",
                        TokenValue::Utf16String(_) => "u",
                        TokenValue::Utf32String(_) => "U",
                        _ => token.encoding_prefix(),
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
                // 6.10.3.2p2 asks for "the spelling of the preprocessing
                // token", which for a punctuator means its own spelling: a
                // multi-character one at all (dropping it turned `a >> b`
                // into `a  b`), and a digraph as the digraph (6.4.6p3 makes
                // `<:` behave as `[` "except for their spelling").
                TokenValue::Special(code) => match token.punctuator(*code) {
                    // The result is a payload -- one `char` per source byte --
                    // and a single-character punctuator already is a byte, so
                    // it goes in directly. Rendering it through a String first
                    // UTF-8-encodes it, which doubled every byte >= 0x80.
                    Punctuator::Byte(b) => result.push(char::from(b)),
                    // A digraph or a longer operator spells itself in ASCII,
                    // where text and payload are the same.
                    Punctuator::Text(t) => result.push_str(&t),
                },
                _ => {}
            }
        }
        result
    }

    /// The comma that separates two variadic arguments.
    ///
    /// It abuts what precedes it: whatever space the source had before it
    /// belongs to the token that follows, which carries its own flag.
    fn variadic_separator(pos: Position) -> Token {
        let mut comma =
            Token::with_value(TokenType::Special, pos, TokenValue::Special(b',' as u32));
        comma.pos.whitespace = false;
        comma.pos.newline = false;
        comma
    }

    /// The variadic arguments as the single token sequence `__VA_ARGS__`
    /// denotes (6.10.3.1p2), with the separating commas restored.
    fn join_variadic_args(args: &[Vec<Token>], start: usize) -> Vec<Token> {
        let mut out: Vec<Token> = Vec::new();
        for (j, arg) in args.iter().enumerate().skip(start) {
            if j > start {
                let pos = arg.first().map_or_else(
                    || out.last().map_or(Position::new(0, 1, 0), |t: &Token| t.pos),
                    |t| t.pos,
                );
                out.push(Self::variadic_separator(pos));
            }
            out.extend(arg.iter().cloned());
        }
        out
    }

    pub(super) fn try_expand_macro(
        &mut self,
        name: &str,
        invoker: &Token,
        iter: &mut TokenCursor,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        let pos = &invoker.pos;
        let invoker_hide = invoker.no_expand.as_ref();

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
                        let open_paren = iter.next()?; // consume '('
                                                       // Returns None having pushed the '(' and everything
                                                       // after it back, so the macro name is emitted plain.
                        let (args, close_hide) =
                            self.collect_macro_args(iter, idents, pos, name, &open_paren)?;
                        // C17 6.10.3.4 by way of Prosser: a function-like
                        // invocation hides what *both* ends of the call were
                        // hiding, not what the name alone was. Propagating the
                        // name's set whole over-hid, because the `)` usually
                        // comes from the file with nothing hidden at all:
                        // `#define f(a) a*g` with `#define g(a) f(a)` made
                        // `f(2)(9)` stop at `2*f(9)` instead of `2*9*g`.
                        let hide = Self::intersect_hide(invoker_hide, close_hide.as_ref());
                        return self.expand_function_macro(&mac, &args, pos, hide.as_ref(), idents);
                    }
                }
            }
            // No paren - don't expand
            return None;
        }

        // Object-like macro
        self.expand_object_macro(&mac, pos, invoker_hide, idents)
    }

    /// Collect arguments for a function-like macro call
    pub(super) fn collect_macro_args(
        &self,
        iter: &mut TokenCursor,
        _idents: &IdentTable,
        macro_pos: &Position,
        macro_name: &str,
        open_paren: &Token,
    ) -> Option<MacroCall> {
        let mut args = Vec::new();
        let mut current_arg = Vec::new();
        // Start at depth 1 because the opening '(' has already been consumed
        // by the caller. This is important for handling multiline macro calls.
        let mut paren_depth = 1;
        let mut found_closing_paren = false;
        // The `)` carries a hide set of its own, and the invocation's result
        // hides only what both ends were hiding.
        let mut close_hide = None;

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
                                args.push(std::mem::take(&mut current_arg));
                            }
                            found_closing_paren = true;
                            close_hide = token.no_expand.clone();
                            break;
                        }
                        // Nested ')' - add to current argument
                        current_arg.push(token);
                    } else if *code == b',' as u32 && paren_depth == 1 {
                        // Argument separator at top level (paren_depth == 1 since we're inside the macro call)
                        args.push(std::mem::take(&mut current_arg));
                    } else {
                        current_arg.push(token);
                    }
                }
                _ => {
                    current_arg.push(token);
                }
            }
        }

        // Unterminated: the whole file went by without a closing ')'.
        //
        // Give the tokens back rather than expanding with whatever was
        // collected. Since expansion became a splice, the scan runs to end of
        // file looking for that ')', so expanding anyway would substitute the
        // rest of the translation unit into the macro and emit the result --
        // `#define BAD f(` followed by `BAD` produced `(()+1)` and deleted
        // everything after it. gcc leaves the macro name in place, and so does
        // this now.
        if !found_closing_paren {
            crate::diag::error_args(
                *macro_pos,
                "unterminated argument list invoking macro \"{0}\"",
                &[macro_name],
            );
            let mut back = vec![open_paren.clone()];
            for (i, arg) in args.into_iter().enumerate() {
                if i > 0 {
                    back.push(Self::variadic_separator(*macro_pos));
                }
                back.extend(arg);
            }
            back.extend(current_arg);
            iter.unread(back);
            return None;
        }

        Some((args, close_hide))
    }

    /// The names hidden at *both* ends of a function-like macro invocation.
    ///
    /// `None` where either end hides nothing, which is the common case and the
    /// reason this is an intersection rather than a union: the closing `)` is
    /// usually a token of the file, hiding nothing at all.
    fn intersect_hide(
        name: Option<&HashSet<String>>,
        close: Option<&HashSet<String>>,
    ) -> Option<HashSet<String>> {
        let (name, close) = (name?, close?);
        let both: HashSet<String> = name.intersection(close).cloned().collect();
        (!both.is_empty()).then_some(both)
    }

    /// Whether a variadic macro's trailing arguments amount to no tokens.
    ///
    /// C23 6.10.5.1 asks this of the arguments *before* expansion, so it is a
    /// token count and not a question about what they expand to: `F(a,)` is
    /// empty, `F(a, EMPTY)` is not, even when `EMPTY` expands to nothing.
    ///
    /// Two shapes mean "no arguments", because `collect_macro_args` yields an
    /// empty vector for `F()` and a one-empty-argument vector for `F(a,)`. It
    /// is deliberately not "all arguments empty": `F(a,,)` denotes a comma and
    /// is *not* empty.
    fn variadic_is_empty(mac: &Macro, args: &[Vec<Token>]) -> bool {
        let va: Vec<_> = args.iter().skip(mac.params.len()).collect();
        va.is_empty() || (va.len() == 1 && va[0].is_empty())
    }

    /// Whether a `##` sits next to `body[i]` in direction `step`, looking past
    /// the `__VA_OPT__` group markers.
    ///
    /// Those markers are not tokens of the replacement list; they say where a
    /// group begins and ends. A paste written against the group -- as in
    /// `a ## __VA_OPT__(b)` -- is adjacent to the token inside it, and stopping
    /// at the marker would silently drop the paste.
    fn paste_adjacent(body: &[MacroToken], i: usize, step: isize) -> bool {
        let mut j = i as isize + step;
        while j >= 0 && (j as usize) < body.len() {
            match body[j as usize].value {
                MacroTokenValue::VaOptStart { .. } | MacroTokenValue::VaOptEnd => j += step,
                MacroTokenValue::Paste => return true,
                _ => return false,
            }
        }
        false
    }

    /// A body slice with parameters replaced by their arguments *as written*.
    ///
    /// This is what `#` sees: 6.10.3.2p2 stringifies the argument's own
    /// spelling, not what it expands to, and `#__VA_OPT__(...)` inherits that.
    fn substitute_unexpanded(
        &self,
        mac: &Macro,
        args: &[Vec<Token>],
        body: &[MacroToken],
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Vec<Token> {
        let mut out = Vec::new();
        for mt in body {
            match &mt.value {
                MacroTokenValue::Param(idx) => {
                    out.extend(args.get(*idx).cloned().unwrap_or_default())
                }
                MacroTokenValue::VaArgs => {
                    out.extend(Self::join_variadic_args(args, mac.params.len()))
                }
                // A nested group's markers contribute nothing of their own, and
                // `#`/`##` inside a stringified group are spelled as written.
                MacroTokenValue::VaOptStart { .. } | MacroTokenValue::VaOptEnd => {}
                MacroTokenValue::Paste => out.push(Token::with_value(
                    TokenType::Special,
                    *pos,
                    TokenValue::Special(SpecialToken::HashHash as u32),
                )),
                _ => out.push(self.macro_token_to_token(mt, pos, idents)),
            }
        }
        out
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

    pub(super) fn expand_function_macro(
        &mut self,
        mac: &Macro,
        args: &[Vec<Token>],
        pos: &Position,
        invoker_hide: Option<&std::collections::HashSet<String>>,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        self.check_macro_arity(mac, args, pos);

        // One fully-replaced form per argument, filled in on first use. Only
        // the unquoted uses go through it: `#x` needs the argument's original
        // spelling and `a##b` needs its unexpanded tokens.
        //
        // Indexed by *parameter*, so it is sized by the parameter list rather
        // than by however many arguments were supplied. A call with too few --
        // already diagnosed by `check_macro_arity`, and then expanded anyway so
        // the rest of the file still makes sense -- otherwise indexed past the
        // end and panicked the compiler.
        let mut expanded_args: Vec<Option<Vec<Token>>> =
            vec![None; args.len().max(mac.params.len())];

        // Arguments are macro-replaced on their own first (C17 6.10.3.1p1:
        // "as if they formed the rest of the preprocessing file"), and are not
        // yet hidden by this macro -- so `#define f(x) (x)` expands `f(f(1))`
        // to `((1))`.

        let mut result = Vec::new();
        let mut i = 0;

        while i < mac.body.len() {
            let mt = &mac.body[i];

            // Check for token pasting
            let next_is_paste = Self::paste_adjacent(&mac.body, i, 1);
            let prev_was_paste = Self::paste_adjacent(&mac.body, i, -1);

            match &mt.value {
                MacroTokenValue::VaOptStart { end, stringify } => {
                    let empty = Self::variadic_is_empty(mac, args);
                    if *stringify {
                        // `#__VA_OPT__(...)`: one string token standing for the
                        // whole group, so the group's own tokens are never
                        // emitted and the scan resumes past it either way.
                        let text = if empty {
                            String::new()
                        } else {
                            let inner = self.substitute_unexpanded(
                                mac,
                                args,
                                &mac.body[i + 1..*end - 1],
                                pos,
                                idents,
                            );
                            self.stringify_arg(&inner, idents)
                        };
                        let mut str_pos = *pos;
                        str_pos.whitespace = mt.whitespace;
                        str_pos.newline = false;
                        result.push(Token::with_value(
                            TokenType::String,
                            str_pos,
                            TokenValue::String(text),
                        ));
                        i = *end;
                        continue;
                    }
                    // The group's content stands only when there are variadic
                    // arguments; otherwise skip past its end entirely.
                    if empty {
                        i = *end;
                        continue;
                    }
                }
                MacroTokenValue::VaOptEnd => {}
                MacroTokenValue::Paste => {
                    // Skip paste markers
                    i += 1;
                    continue;
                }
                MacroTokenValue::Stringify(idx) => {
                    // Stringify the argument per C99 6.10.3.2p2.
                    //
                    // `#__VA_ARGS__` stringifies the *whole* variadic token
                    // sequence, commas included -- 6.10.3.1p2 makes
                    // __VA_ARGS__ that sequence, not its first element.
                    // Taking args[idx] alone silently dropped everything
                    // after the first comma, so `V(1,2,3)` came out `"1"`.
                    let arg = if *idx >= mac.params.len() {
                        Self::join_variadic_args(args, *idx)
                    } else {
                        args.get(*idx).cloned().unwrap_or_default()
                    };
                    let text = self.stringify_arg(&arg, idents);
                    // The result is one token of the expansion, not the
                    // invocation: it takes the `#` operator's spacing, and it
                    // never starts a line. Copying `*pos` verbatim gave a
                    // line-initial `S(x)` a stringified token flagged as
                    // beginning a line, which `-E` then broke a line on.
                    let mut str_pos = *pos;
                    str_pos.whitespace = mt.whitespace;
                    str_pos.newline = false;
                    result.push(Token::with_value(
                        TokenType::String,
                        str_pos,
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
                            // Raw argument tokens, straight from the file. An
                            // argument may span lines, and a token that still
                            // says it begins one would be read back as
                            // starting a directive.
                            result.extend(arg.into_iter().map(clear_newline));
                        }
                    } else {
                        // Expand the argument once, however many times the body
                        // names the parameter. C17 6.10.3.1p1 replaces a
                        // parameter with the *same* fully-replaced sequence
                        // each time, so re-running the pass per occurrence
                        // could only produce the same tokens again -- and it
                        // dominated the cost of any macro that used a parameter
                        // more than once.
                        if expanded_args[*idx].is_none() {
                            let expanded = self.preprocess(arg, idents);
                            let mut out = Vec::with_capacity(expanded.len());
                            for mut tok in expanded {
                                if matches!(tok.typ, TokenType::StreamBegin | TokenType::StreamEnd)
                                {
                                    continue;
                                }
                                // Re-point the token at the invocation for
                                // diagnostics, but keep whether it was preceded
                                // by white space: that belongs to the
                                // argument's own spelling, and 6.10.3.2p2 makes
                                // `#` reproduce it. Taking `whitespace` from
                                // the invocation site gave every expanded token
                                // one, so the two-level `XSTR(x)`/`STR(x)`
                                // idiom turned `1+2` into `"1 + 2"` while the
                                // direct `STR(1+2)` was right.
                                let whitespace = tok.pos.whitespace;
                                tok.pos = *pos;
                                tok.pos.newline = false;
                                tok.pos.whitespace = whitespace;
                                out.push(tok);
                            }
                            expanded_args[*idx] = Some(out);
                        }
                        result.extend(expanded_args[*idx].as_ref().unwrap().iter().cloned());
                    }
                }
                MacroTokenValue::VaArgs => {
                    // Variadic arguments
                    let start = mac.params.len();

                    let va_start = result.len();
                    for (j, arg) in args.iter().enumerate().skip(start) {
                        if j > start {
                            result.push(Self::variadic_separator(*pos));
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
                                // Keep each token's own spacing. Overwriting
                                // it with the macro body's position gave every
                                // argument a leading space, so `X(1,2,3)`
                                // expanded to `1 , 2 , 3`.
                                let whitespace = tok.pos.whitespace;
                                tok.pos = *pos;
                                tok.pos.newline = false;
                                tok.pos.whitespace = whitespace;
                                result.push(tok);
                            }
                        }
                    }
                    // The sequence as a whole is spaced as `__VA_ARGS__` was
                    // in the macro body -- `mt.whitespace`, the same source
                    // every other body token uses through
                    // `macro_token_to_token`. Taking it from `pos` used the
                    // *invocation's* spacing instead, so `#define F(...) x
                    // __VA_ARGS__` expanded `F(*p)` to `x*p`.
                    if let Some(first) = result.get_mut(va_start) {
                        first.pos.whitespace = mt.whitespace;
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

        // Hide before handing the expansion back.
        //
        // The result is rescanned by the caller's loop, which is where a
        // self-referential macro would recurse -- so hiding has to happen
        // before it is handed back, not after. glibc's enum-and-macro idiom,
        // `#define MSG_DONTROUTE MSG_DONTROUTE`, is the shape that catches
        // this. Hiding here also propagates: a nested expansion sees an
        // invoking token that already carries this set.
        //
        // Substitution and pasting are both finished by now, which is the
        // other half of the requirement -- a name that `##` built has to be
        // hidden too.
        hide_in_expansion(&mut result, &mac.name, invoker_hide);

        // No rescan here: the caller pushes this back in front of the cursor,
        // so it is rescanned by the same loop that produced it.
        let mut filtered = result;

        // The FIRST token of a macro expansion stands where the invocation
        // stood: it inherits both the spacing before it and whether it begins
        // a line. Body tokens carry neither, since they were written somewhere
        // else entirely.
        //
        // The line flag used to be dropped, so an expansion at the start of a
        // line ran onto the previous one in `-E` output -- `M int b;` came out
        // as `int a; 42 int b;` where gcc breaks the line. Stringification hid
        // that for its own case by copying the invocation position wholesale,
        // which then made a `#x` result claim to begin a line even mid-line.
        if let Some(first) = filtered.first_mut() {
            first.pos.whitespace = pos.whitespace;
            first.pos.newline = pos.newline;
        }

        Some(filtered)
    }

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

        let mut result = self.paste_one(left, &right[0], pos, idents);

        // Add remaining right tokens. Only `right[0]` took part in the paste,
        // so these still carry the file's line flags.
        result.extend(right.iter().skip(1).cloned().map(clear_newline));

        result
    }

    /// Paste exactly two tokens.
    ///
    /// C17 6.10.3.3p3: the result has to be a single preprocessing token, and
    /// a paste that does not produce one is a constraint violation requiring a
    /// diagnostic. This used to concatenate the two spellings, re-lex, and keep
    /// every token that fell out -- so `cat(+,-)` quietly produced two tokens,
    /// and `cat(/,/)` produced `//`, which the lexer read as a comment: both
    /// operands and the tokens around them vanished from
    /// `int y = 1 cat(/,/) 2;` with nothing reported.
    ///
    /// The operand *types* decide whether a paste can be valid at all, before
    /// any spelling is built. Only an identifier, a pp-number or a punctuator
    /// can take part; a string or character literal can only ever be the right
    /// operand of an encoding prefix.
    fn paste_one(
        &self,
        left: &Token,
        right: &Token,
        pos: &Position,
        idents: &mut IdentTable,
    ) -> Vec<Token> {
        // `L ## "x"` and friends: the prefix and the literal are one token.
        if let Some(token) = Self::paste_encoding_prefix(left, right, pos, idents) {
            return vec![token];
        }

        if !Self::pasteable(left) || !Self::pasteable(right) {
            self.reject_paste(left, right, pos, idents);
            return vec![clear_newline(left.clone()), clear_newline(right.clone())];
        }

        let combined = format!("{}{}", show_token(left, idents), show_token(right, idents));

        // The spelling is built from two tokens that are each valid on their
        // own, so re-lexing it is a way of *constructing* the result, not of
        // deciding whether there is one. That is what the count below decides.
        let stream_id = self.paste_stream();
        let tokens: Vec<_> = {
            let mut tokenizer = Tokenizer::new(combined.as_bytes(), stream_id, idents);
            tokenizer.tokenize()
        }
        .into_iter()
        .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
        .map(|mut t| {
            t.pos = *pos;
            t.pos.newline = false;
            t
        })
        .collect();

        if tokens.len() == 1 {
            return tokens;
        }

        // Zero tokens means the spelling lexed as a comment (`/` ## `/`); more
        // than one means it never joined.
        self.reject_paste(left, right, pos, idents);
        vec![clear_newline(left.clone()), clear_newline(right.clone())]
    }

    /// Whether a token can be an operand of `##` at all.
    fn pasteable(token: &Token) -> bool {
        matches!(
            token.typ,
            TokenType::Ident | TokenType::Number | TokenType::Special
        )
    }

    /// `L`, `u`, `U` or `u8` pasted onto a literal, which C17 6.4.5 spells as
    /// one token.
    fn paste_encoding_prefix(
        left: &Token,
        right: &Token,
        pos: &Position,
        idents: &IdentTable,
    ) -> Option<Token> {
        if left.typ != TokenType::Ident {
            return None;
        }
        let TokenValue::Ident(id) = &left.value else {
            return None;
        };
        let prefix = idents.get_opt(*id)?;
        let value = match (prefix, &right.value) {
            ("L", TokenValue::String(s)) => TokenValue::WideString(s.clone()),
            ("u", TokenValue::String(s)) => TokenValue::Utf16String(s.clone()),
            ("U", TokenValue::String(s)) => TokenValue::Utf32String(s.clone()),
            ("u8", TokenValue::String(s)) => TokenValue::String(s.clone()),
            ("L", TokenValue::Char(c)) => TokenValue::WideChar(c.clone()),
            ("u", TokenValue::Char(c)) => TokenValue::Utf16Char(c.clone()),
            ("U", TokenValue::Char(c)) => TokenValue::Utf32Char(c.clone()),
            _ => return None,
        };
        let typ = match &value {
            TokenValue::WideString(_) => TokenType::WideString,
            TokenValue::Utf16String(_) => TokenType::Utf16String,
            TokenValue::Utf32String(_) => TokenType::Utf32String,
            TokenValue::String(_) => TokenType::String,
            TokenValue::WideChar(_) => TokenType::WideChar,
            TokenValue::Utf16Char(_) => TokenType::Utf16Char,
            _ => TokenType::Utf32Char,
        };
        let mut token = Token::with_value(typ, *pos, value);
        token.pos.newline = false;
        // `u8"..."` is a narrow string in every respect but its spelling, so
        // the prefix lives on the spelling flag rather than in the token type.
        // Without it the paste produced a plain `"y"` and the `u8` vanished.
        if prefix == "u8" {
            token.spelling = Spelling::Utf8Prefix;
        }
        Some(token)
    }

    /// Report a `##` whose result is not a single preprocessing token.
    ///
    /// The operands are then emitted side by side, unpasted, which is what gcc
    /// does and keeps the rest of the line meaningful.
    fn reject_paste(&self, left: &Token, right: &Token, pos: &Position, idents: &IdentTable) {
        crate::diag::error_args(
            *pos,
            "pasting \"{0}\" and \"{1}\" does not give a valid preprocessing token",
            &[&show_token(left, idents), &show_token(right, idents)],
        );
    }

    fn expand_object_macro(
        &mut self,
        mac: &Macro,
        pos: &Position,
        invoker_hide: Option<&std::collections::HashSet<String>>,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
        if mac.body.is_empty() {
            return Some(vec![]);
        }

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

        // Hide before handing the expansion back; see `expand_function_macro` for the full rule.
        hide_in_expansion(&mut result, &mac.name, invoker_hide);

        // No rescan here: the caller pushes this back in front of the cursor,
        // so it is rescanned by the same loop that produced it.
        let mut filtered = result;

        // The first token inherits the invocation's spacing and line flag; see `expand_function_macro`.
        if let Some(first) = filtered.first_mut() {
            first.pos.whitespace = pos.whitespace;
            first.pos.newline = pos.newline;
        }

        Some(filtered)
    }

    /// Every macro definition in force, as the `#define` that would make it,
    /// sorted by name.
    ///
    /// Sorted because `macros` is a `HashMap` and its iteration order differs
    /// per process: an unsorted dump would make `-dM` output differ run to run,
    /// which is exactly what the determinism rule exists to stop.
    ///
    /// The dynamic builtins -- `__FILE__`, `__LINE__`, `__COUNTER__`,
    /// `__has_include` and the rest -- are omitted, as gcc omits them. They
    /// have no replacement list to print; what they expand to is a function of
    /// where they appear.
    pub fn macro_definitions(&self, idents: &mut IdentTable) -> Vec<String> {
        let mut names: Vec<&String> = self
            .macros
            .iter()
            .filter(|(_, mac)| mac.builtin.is_none())
            .map(|(name, _)| name)
            .collect();
        names.sort_unstable();

        names
            .into_iter()
            .map(|name| {
                // Cloned out so the table is not borrowed while rendering,
                // which needs `&mut IdentTable`.
                let mac = self.macros[name].clone();
                self.render_definition(&mac, idents)
            })
            .collect()
    }

    /// One `#define` line for one macro.
    fn render_definition(&self, mac: &Macro, idents: &mut IdentTable) -> String {
        let mut out = format!("#define {}", mac.name);

        if mac.is_function {
            out.push('(');
            for (i, p) in mac.params.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                out.push_str(&p.name);
            }
            if mac.is_variadic {
                if !mac.params.is_empty() {
                    out.push(',');
                }
                match &mac.variadic_name {
                    // The GNU spelling names the trailing arguments.
                    Some(name) => {
                        out.push_str(name);
                        out.push_str("...");
                    }
                    None => out.push_str("..."),
                }
            }
            out.push(')');
        }

        // gcc writes a trailing space even for an empty replacement list, and
        // the space is what separates the name from the body in every other
        // case, so it is unconditional here too.
        out.push(' ');
        for (i, mt) in mac.body.iter().enumerate() {
            if i > 0 && mt.whitespace {
                out.push(' ');
            }
            out.push_str(&self.render_body_token(mac, mt, idents));
        }
        out
    }

    /// One body token, with the parameter references put back as names.
    fn render_body_token(&self, mac: &Macro, mt: &MacroToken, idents: &mut IdentTable) -> String {
        let variadic = || match &mac.variadic_name {
            Some(name) => name.clone(),
            None => "__VA_ARGS__".to_string(),
        };
        match &mt.value {
            MacroTokenValue::Param(idx) => mac
                .params
                .get(*idx)
                .map(|p| p.name.clone())
                .unwrap_or_else(variadic),
            MacroTokenValue::Stringify(idx) => format!(
                "#{}",
                mac.params
                    .get(*idx)
                    .map(|p| p.name.clone())
                    .unwrap_or_else(variadic)
            ),
            MacroTokenValue::Paste => "##".to_string(),
            MacroTokenValue::VaArgs => variadic(),
            // The group markers are flat, so the opening one carries the `(`
            // and the closing one the `)`. Without these arms both fell to the
            // catch-all below, where `macro_token_to_token` has nothing to
            // build from and `-dM` printed `a <ident?>, __VA_ARGS__<ident?>`.
            MacroTokenValue::VaOptStart { stringify, .. } => {
                if *stringify {
                    "#__VA_OPT__(".to_string()
                } else {
                    "__VA_OPT__(".to_string()
                }
            }
            MacroTokenValue::VaOptEnd => ")".to_string(),
            // An ordinary body token: round-trip it through a real token so the
            // spelling is the lexer's, not a second guess at one.
            _ => {
                let token = self.macro_token_to_token(mt, &Position::default(), idents);
                show_token(&token, idents)
            }
        }
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
                TokenType::HeaderName => TokenValue::HeaderName(s.clone()),
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

        let mut token = Token::with_value(mt.typ, new_pos, value);
        token.spelling = mt.spelling;
        token
    }

    pub(super) fn expand_builtin(
        &mut self,
        builtin: BuiltinMacro,
        pos: &Position,
        mac: &Macro,
        iter: &mut TokenCursor,
        idents: &mut IdentTable,
    ) -> Option<Vec<Token>> {
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
                    TokenValue::String(literal_payload(&effective_file)),
                )])
            }
            BuiltinMacro::Date => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(literal_payload(&self.compile_date)),
            )]),
            BuiltinMacro::Time => Some(vec![Token::with_value(
                TokenType::String,
                *pos,
                TokenValue::String(literal_payload(&self.compile_time)),
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
                            let open_paren = iter.next()?;
                            let args = self
                                .collect_macro_args(iter, idents, pos, &mac.name, &open_paren)
                                .map(|(args, _)| args)
                                .unwrap_or_default();
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
                            let open_paren = iter.next()?;
                            let args = self
                                .collect_macro_args(iter, idents, pos, &mac.name, &open_paren)
                                .map(|(args, _)| args)
                                .unwrap_or_default();
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
}
