//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Preprocessing directives, and the include resolution that `#include`
// exists to drive.
//
// A child module of `preprocess`, so these still reach `Preprocessor`'s
// private fields and the parent's helpers exactly as they did inline.
// `pub(super)` marks only what the parent, a sibling, or the tests call.
//

use super::*;

impl<'a> Preprocessor<'a> {
    /// Handle a preprocessor directive
    /// The directives that survive into an already-preprocessed file.
    ///
    /// GCC's libcpp marks these `IN_I`. Every one of them is dispatched by
    /// `handle_directive`; naming one that is not would send it to the
    /// unknown-directive arm while claiming it was supported.
    fn survives_preprocessing(id: crate::strings::StringId) -> bool {
        matches!(
            id,
            crate::kw::DEFINE
                | crate::kw::UNDEF
                | crate::kw::PRAGMA
                | crate::kw::PP_IDENT
                | crate::kw::SCCS
        )
    }

    /// Every directive `handle_directive` dispatches.
    ///
    /// Kept beside the `match` it mirrors: a directive added there and not
    /// here would silently stop being diagnosed as a stray `#` in a `.i`.
    fn is_known_directive(id: crate::strings::StringId) -> bool {
        Self::survives_preprocessing(id)
            || matches!(
                id,
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
    }

    pub(super) fn handle_directive(
        &mut self,
        iter: &mut TokenCursor,
        hash_token: &Token,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
    ) {
        // No assertion about the pushback being empty here. It used to say so,
        // on the premise that file provenance implies nothing is pending --
        // which stopped being true once recovery began unreading file tokens.
        // What the line-oriented helpers below actually need is that this token
        // came from the file, which is what the caller already checked.

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
        // file silently loses its layout. A *known* directive outside the
        // allowlist leaves the `#` as a stray token, which is what GCC
        // diagnoses.
        //
        // An identifier that names no directive at all is deliberately not
        // included: GCC reports `#nonsense` the same way whether the file is
        // preprocessed or not, so it goes to the unknown-directive arm below
        // in both modes rather than becoming a stray `#` in one of them.
        if self.preprocessed
            && Self::is_known_directive(directive_id)
            && !Self::survives_preprocessing(directive_id)
        {
            // One diagnostic naming the cause, rather than GCC's cascade of
            // follow-on parse errors from feeding the rest of the line to the
            // parser.
            diag::error(hash_token.pos, "stray '#' in program");
            self.skip_to_eol(iter);
            return;
        }

        match directive_id {
            crate::kw::DEFINE => self.handle_define(iter, idents, hash_token.pos),
            crate::kw::UNDEF => self.handle_undef(iter, idents, hash_token.pos),
            crate::kw::IFDEF => self.handle_ifdef(iter, idents, hash_token.pos),
            crate::kw::IFNDEF => self.handle_ifndef(iter, idents, hash_token.pos),
            crate::kw::IF => self.handle_if(iter, idents, hash_token.pos),
            crate::kw::ELIF => self.handle_elif(iter, idents, hash_token.pos),
            crate::kw::ELSE => self.handle_else(iter, hash_token.pos),
            crate::kw::ENDIF => self.handle_endif(iter, hash_token.pos),
            crate::kw::INCLUDE => self.handle_include(iter, output, idents, hash_token, false),
            crate::kw::INCLUDE_NEXT => self.handle_include(iter, output, idents, hash_token, true),
            crate::kw::PP_ERROR => self.handle_error(iter, &hash_token.pos, idents),
            crate::kw::WARNING => self.handle_warning(iter, &hash_token.pos, idents),
            crate::kw::PRAGMA => self.handle_pragma(iter, output, idents),
            crate::kw::LINE => self.handle_line(iter, idents, hash_token.pos),
            // `#ident` and `#sccs` carry a version string for the object file.
            // c17 records nothing, but they are directives it knows, so they
            // are consumed rather than reported as unknown -- GCC is silent
            // about both, and `survives_preprocessing` names them, which was
            // untrue while they fell through to the arm below.
            crate::kw::PP_IDENT | crate::kw::SCCS => self.skip_to_eol(iter),
            _ => {
                // Unknown directive.
                //
                // In assembly, `#` introduces a comment, so a line that names
                // no directive is prose rather than a mistake -- `# save the
                // frame pointer` is ordinary in a `.S` file. GCC is silent
                // about those, and warning on each one buried real
                // diagnostics.
                if !self.is_skipping() && self.lexer_mode != LexerMode::Assembly {
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

    /// Consume a `# N ["file" [flags]]` linemarker and record the attribution
    /// it establishes for the text that follows.
    ///
    /// `number` is the directive-name token, which the caller has already
    /// taken from the iterator and found to be a number rather than an
    /// identifier. C17 has no such directive; this is the GCC form that
    /// `c17 -E` itself writes, and `#line` is deliberately *not* routed here
    /// (GCC honors only this form in a preprocessed file, and so does c17).
    fn handle_linemarker(&mut self, iter: &mut TokenCursor, number: &Token) {
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
                    target = diag::find_or_add_stream(&payload_text(name));
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
    /// The next token, only if it is on the directive's own line.
    ///
    /// A directive's operand cannot be on the next line. There is no newline
    /// token in the stream, so a bare `next()` silently reaches into the
    /// following line and `skip_to_eol` then eats the rest of it -- deleting a
    /// line of source. The same check already guards the directive *name* in
    /// `handle_directive`; this is that check applied to the operands, where it
    /// was missing.
    fn next_on_line(&self, iter: &mut TokenCursor) -> Option<Token> {
        match iter.peek() {
            Some(token) if !token.pos.newline => iter.next(),
            _ => None,
        }
    }

    /// The macro name a `#define`, `#undef`, `#ifdef` or `#ifndef` operates on.
    ///
    /// `None` means the directive was malformed and has been diagnosed; the
    /// caller must not go on to define, undefine or test anything, but must
    /// still keep the conditional stack balanced where that applies.
    fn macro_name_operand(
        &self,
        iter: &mut TokenCursor,
        idents: &IdentTable,
        directive: &str,
        pos: Position,
    ) -> Option<String> {
        let Some(token) = self.next_on_line(iter) else {
            diag::error_args(pos, "no macro name given in #{0} directive", &[directive]);
            return None;
        };
        match &token.value {
            TokenValue::Ident(id) if token.typ == TokenType::Ident => {
                Some(idents.get_opt(*id)?.to_string())
            }
            _ => {
                diag::error(token.pos, &gettext("macro names must be identifiers"));
                None
            }
        }
    }

    /// Drain the rest of the directive's line, warning if there was any.
    ///
    /// C17 6.10p1 gives `#else` and `#endif` no operands at all, and gcc warns
    /// about anything after them -- usually a stale `#endif MACRO` left from
    /// before comments were the convention. These used to be eaten in silence.
    fn warn_extra_tokens(&self, iter: &mut TokenCursor, directive: &str) {
        if iter.peek().is_some_and(|t| !t.pos.newline) {
            let pos = iter.peek().map(|t| t.pos).unwrap_or_default();
            diag::warning_args(pos, "extra tokens at end of #{0} directive", &[directive]);
        }
        self.skip_to_eol(iter);
    }

    fn skip_to_eol(&self, iter: &mut TokenCursor) {
        while let Some(token) = iter.peek() {
            if token.pos.newline {
                break;
            }
            iter.next();
        }
    }

    /// Handle #define
    pub(super) fn handle_define(
        &mut self,
        iter: &mut TokenCursor,
        idents: &IdentTable,
        pos: Position,
    ) {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Get macro name
        let name_pos = iter.peek().map(|t| t.pos).unwrap_or(pos);
        let Some(name) = self.macro_name_operand(iter, idents, "define", pos) else {
            self.skip_to_eol(iter);
            return;
        };

        // C17 6.10.8p4: `defined` is not available to be defined. Accepting it
        // put a macro in the table that `#if` can never see, because the
        // operator is recognised before expansion.
        if name == "defined" {
            diag::error(
                name_pos,
                &gettext("\"defined\" cannot be used as a macro name"),
            );
            self.skip_to_eol(iter);
            return;
        }

        // Check if function-like macro (immediate '(' without whitespace)
        let mut params: Vec<MacroParam> = Vec::new();
        let mut is_function = false;
        // A parameter list that does not parse defines nothing: continuing
        // with a guessed list is how `#define F(a,a) a` and
        // `#define H(x + y) x` came to be accepted.
        let mut malformed_params = false;
        let mut closed_params = false;
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
                        while let Some(param_tok) = self.next_on_line(iter) {
                            match &param_tok.value {
                                TokenValue::Special(c) if *c == b')' as u32 => {
                                    closed_params = true;
                                    break;
                                }
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
                                    while let Some(t) = self.next_on_line(iter) {
                                        if let TokenValue::Special(c) = &t.value {
                                            if *c == b')' as u32 {
                                                closed_params = true;
                                                break;
                                            }
                                        }
                                    }
                                    break;
                                }
                                TokenValue::Ident(id) => {
                                    if let Some(param_name) = idents.get_opt(*id) {
                                        // C17 6.10.3p6: the parameters have to
                                        // be distinct. A repeat used to be
                                        // pushed anyway, and since substitution
                                        // matches the *first* one by name,
                                        // `#define F(a,a) a` silently made
                                        // `F(1,2)` expand to `1`.
                                        if params.iter().any(|p| p.name == param_name) {
                                            diag::error_args(
                                                param_tok.pos,
                                                "duplicate macro parameter \"{0}\"",
                                                &[param_name],
                                            );
                                            malformed_params = true;
                                        }
                                        params.push(MacroParam {
                                            name: param_name.to_string(),
                                            index: param_index,
                                        });
                                        param_index += 1;
                                        ident_immediately_before = true;
                                    }
                                }
                                // Anything else cannot be a parameter. This was
                                // a bare `_ => {}`, so `#define H(x + y) x`
                                // quietly defined a two-parameter macro.
                                _ => {
                                    diag::error_args(
                                        param_tok.pos,
                                        "expected ',' or ')' in macro parameter list, found \"{0}\"",
                                        &[&self.token_to_string(&param_tok, idents)],
                                    );
                                    malformed_params = true;
                                }
                            }
                        }
                        if !closed_params {
                            diag::error(
                                name_pos,
                                &gettext("expected ')' at end of macro parameter list"),
                            );
                            malformed_params = true;
                        }
                    }
                }
            }
        }
        if malformed_params {
            // The parameter list was rejected, so the replacement list is not a
            // macro body -- it is not anything. Returning without draining the
            // line left it in the stream to be emitted as ordinary code, after
            // the directive that produced it had already been diagnosed.
            self.skip_to_eol(iter);
            return;
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
                diag::warning_args(name_pos, "'{0}' redefined: {1}", &[&name.to_string(), why]);
            }
        }

        self.define_macro(mac);
    }

    /// Handle #undef
    fn handle_undef(&mut self, iter: &mut TokenCursor, idents: &IdentTable, pos: Position) {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        if let Some(name) = self.macro_name_operand(iter, idents, "undef", pos) {
            self.undef_macro(&name);
        }

        self.skip_to_eol(iter);
    }

    /// Handle #ifdef
    fn handle_ifdef(&mut self, iter: &mut TokenCursor, idents: &IdentTable, pos: Position) {
        // A malformed operand still pushes a group, so the matching
        // `#endif` closes something and one bad directive does not cascade
        // into a run of "#endif without #if". The group is skipped, since
        // nothing was established about the name.
        let name = self.macro_name_operand(iter, idents, "ifdef", pos);
        let take_branch = match &name {
            Some(name) => self.is_defined(name),
            None => false,
        };

        self.skip_to_eol(iter);
        self.push_conditional(take_branch, pos);
    }

    /// Handle #ifndef
    fn handle_ifndef(&mut self, iter: &mut TokenCursor, idents: &IdentTable, pos: Position) {
        // A malformed operand still pushes a group, so the matching
        // `#endif` closes something and one bad directive does not cascade
        // into a run of "#endif without #if". The group is skipped, since
        // nothing was established about the name.
        let name = self.macro_name_operand(iter, idents, "ifndef", pos);
        let take_branch = match &name {
            Some(name) => !self.is_defined(name),
            None => false,
        };

        self.skip_to_eol(iter);
        self.push_conditional(take_branch, pos);
    }

    /// Handle #if
    fn handle_if(&mut self, iter: &mut TokenCursor, idents: &mut IdentTable, pos: Position) {
        let tokens = self.collect_to_eol(iter);
        let value = if self.is_skipping() {
            false
        } else {
            // Expand macros before evaluation (per C standard)
            let expanded = self.expand_if_tokens(&tokens, idents);
            self.evaluate_expression(&expanded, idents, pos)
        };

        self.push_conditional(value, pos);
    }

    /// Handle #elif
    fn handle_elif(&mut self, iter: &mut TokenCursor, idents: &mut IdentTable, pos: Position) {
        let tokens = self.collect_to_eol(iter);

        // C17 6.10.1: a group runs `#if`, then any `#elif`s, then at most one
        // `#else`. Neither of these was checked, so a stray `#elif` did nothing
        // and an `#elif` after `#else` silently turned the group `Done`,
        // truncating the `#else` body it had already started emitting.
        let should_eval = match self.cond_stack.last() {
            None => {
                diag::error(pos, &gettext("#elif without #if"));
                return;
            }
            Some(cond) if cond.seen_else => {
                diag::error(pos, &gettext("#elif after #else"));
                return;
            }
            Some(cond) => cond.state == CondState::Skipping && !cond.had_true,
        };

        let expr_value = if should_eval {
            // Expand macros before evaluation (per C standard)
            let expanded = self.expand_if_tokens(&tokens, idents);
            self.evaluate_expression(&expanded, idents, pos)
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
    fn handle_else(&mut self, iter: &mut TokenCursor, pos: Position) {
        self.warn_extra_tokens(iter, "else");

        let Some(cond) = self.cond_stack.last_mut() else {
            diag::error(pos, &gettext("#else without #if"));
            return;
        };
        if cond.seen_else {
            diag::error(pos, &gettext("#else after #else"));
            return;
        }
        cond.seen_else = true;
        {
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
    fn handle_endif(&mut self, iter: &mut TokenCursor, pos: Position) {
        self.warn_extra_tokens(iter, "endif");
        if self.cond_stack.pop().is_none() {
            diag::error(pos, &gettext("#endif without #if"));
        }
    }

    /// Handle #include
    fn handle_include(
        &mut self,
        iter: &mut TokenCursor,
        output: &mut Vec<Token>,
        idents: &mut IdentTable,
        hash_token: &Token,
        is_include_next: bool,
    ) {
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
                seen_else: false,
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

        // A header name the lexer already recognised (C99 6.4.7): one token,
        // delimiters included, with nothing inside it reinterpreted.
        if let TokenValue::HeaderName(h) = &tokens[0].value {
            let spelled = payload_text(h);
            let is_system = spelled.starts_with('<');
            let name = spelled
                .strip_prefix(['<', '"'])
                .and_then(|r| r.strip_suffix(['>', '"']))
                .unwrap_or(&spelled);
            return (name.to_string(), is_system);
        }

        // Otherwise the header name came out of a macro expansion, and has to
        // be reassembled from whatever tokens it expanded to.

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
            return (payload_text(s), false);
        }

        // Fallback: try to reconstruct from tokens
        let mut filename = String::new();
        for token in tokens {
            filename.push_str(&self.token_to_string(token, idents));
        }
        (filename, false)
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
    pub(super) fn detect_include_guard(content: &[u8]) -> Option<String> {
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
        // Whatever the file left open, it left open. The stack is swapped out
        // around an inclusion so a header cannot close one of the includer's
        // groups, which also meant an unterminated `#if` in a header was
        // discarded here rather than reported.
        self.report_unterminated_conditionals();
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
        // Whatever the file left open, it left open. The stack is swapped out
        // around an inclusion so a header cannot close one of the includer's
        // groups, which also meant an unterminated `#if` in a header was
        // discarded here rather than reported.
        self.report_unterminated_conditionals();
        self.cond_stack = saved_cond_stack;
    }

    /// Handle #error
    fn handle_error(&mut self, iter: &mut TokenCursor, pos: &Position, idents: &IdentTable) {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        let tokens = self.collect_to_eol(iter);
        let msg = self.tokens_to_message(&tokens, idents);
        diag::error_args(*pos, "#error {0}", &[&msg.to_string()]);
    }

    /// Handle #warning
    fn handle_warning(&mut self, iter: &mut TokenCursor, pos: &Position, idents: &IdentTable) {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        let tokens = self.collect_to_eol(iter);
        let msg = self.tokens_to_message(&tokens, idents);
        diag::warning_args(*pos, "#warning {0}", &[&msg.to_string()]);
    }

    /// Handle #pragma
    fn handle_pragma(
        &mut self,
        iter: &mut TokenCursor,
        output: &mut Vec<Token>,
        idents: &IdentTable,
    ) {
        if self.is_skipping() {
            self.skip_to_eol(iter);
            return;
        }

        // Check for #pragma once and #pragma STDC
        if let Some(token) = iter.peek() {
            if let TokenValue::Ident(id) = &token.value {
                if let Some(name) = idents.get_opt(*id) {
                    if name == "pack" {
                        // A directive handler pulls its own tokens, so they
                        // never pass the remap in the main loop. Attribute the
                        // position once, here, rather than at the one use that
                        // needed it: `parse_pack_body` reports five diagnostics
                        // from this position, and leaving it physical made a
                        // malformed pragma cite the `.i` while the error on the
                        // very next line cited the original file.
                        let pos = self.remap_pos(token.pos);
                        iter.next(); // consume "pack"
                        if let Some(action) = self.parse_pack_pragma(iter, idents, pos) {
                            // The parser decides layout, so the pragma has to
                            // reach it. It travels as a marker in the stream
                            // because that is the only ordering that survives
                            // include splicing.
                            let mut marker = Token::new(TokenType::Pragma, pos);
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
    pub(super) fn handle_pragma_operator(
        &mut self,
        iter: &mut TokenCursor,
        output: &mut Vec<Token>,
    ) {
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
    fn handle_line(
        &mut self,
        iter: &mut TokenCursor,
        idents: &mut IdentTable,
        directive_pos: Position,
    ) {
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

    /// Evaluate __has_include
    pub(super) fn eval_has_include(&self, args: &[Vec<Token>], idents: &IdentTable) -> bool {
        if args.is_empty() {
            return false;
        }

        let (filename, is_system) = self.parse_include_path(&args[0], idents);
        self.find_include_file(&filename, is_system, false)
            .is_some()
    }
}
