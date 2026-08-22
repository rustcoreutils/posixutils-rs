//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unit tests for the lexer. Attached as a child module by `lexer.rs`, so it
// still reaches that module's private items exactly as an inline
// `mod tests` did.
//

#[test]
fn test_replace_trigraphs() {
    // All nine sequences of C17 5.2.1.1.
    assert_eq!(
        replace_trigraphs(b"??=??(??/??)??'??<??!??>??-").as_ref(),
        br"#[\]^{|}~"
    );
    // A buffer with no trigraph is returned untouched.
    assert!(matches!(
        replace_trigraphs(b"int main(void) { return 0; }"),
        std::borrow::Cow::Borrowed(_)
    ));
    // `??` not followed by a trigraph character stays literal — this is
    // the case that makes the feature opt-in.
    assert_eq!(replace_trigraphs(b"What??!").as_ref(), b"What|");
    assert_eq!(replace_trigraphs(b"What??x").as_ref(), b"What??x");
    assert_eq!(replace_trigraphs(b"a??").as_ref(), b"a??");
    // ...and is borrowed, not copied: a `??` that forms no trigraph must
    // not cost an allocation, or the doc's promise is only half true.
    assert!(matches!(
        replace_trigraphs(b"puts(\"Really??\");"),
        std::borrow::Cow::Borrowed(_)
    ));
    assert!(matches!(
        replace_trigraphs(b"a??"),
        std::borrow::Cow::Borrowed(_)
    ));
    // Overlapping question marks: only a complete `??x` is replaced.
    assert_eq!(replace_trigraphs(b"???=").as_ref(), b"?#");
}

#[test]
fn test_literal_encoding_prefixes() {
    let mut strings = StringTable::new();
    let src = br#"u8"a" u"b" U"c" L"d" u'e' U'f' L'g' "h" 'i'"#;
    let tokens = Tokenizer::new(src, 0, &mut strings).tokenize();
    let kinds: Vec<TokenType> = tokens
        .iter()
        .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
        .map(|t| t.typ)
        .collect();
    assert_eq!(
        kinds,
        vec![
            // u8"..." has type char[], so it is an ordinary narrow string.
            TokenType::String,
            TokenType::Utf16String,
            TokenType::Utf32String,
            TokenType::WideString,
            TokenType::Utf16Char,
            TokenType::Utf32Char,
            TokenType::WideChar,
            TokenType::String,
            TokenType::Char,
        ]
    );
}

#[test]
fn test_u8_prefix_only_applies_to_strings() {
    // There is no `u8'x'` character constant in C11, so `u8` before a
    // quote must stay an identifier.
    let mut strings = StringTable::new();
    let tokens = Tokenizer::new(b"u8'x'", 0, &mut strings).tokenize();
    let kinds: Vec<TokenType> = tokens
        .iter()
        .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
        .map(|t| t.typ)
        .collect();
    assert_eq!(kinds, vec![TokenType::Ident, TokenType::Char]);
}
use super::*;

fn tokenize_str(input: &str) -> (Vec<Token>, StringTable) {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(input.as_bytes(), 0, &mut strings);
    let tokens = tokenizer.tokenize();
    (tokens, strings)
}

#[test]
fn test_simple_tokens() {
    let (tokens, idents) = tokenize_str("int main");
    // StreamBegin, "int", "main", StreamEnd
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].typ, TokenType::StreamBegin);
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(tokens[2].typ, TokenType::Ident);
    assert_eq!(tokens[3].typ, TokenType::StreamEnd);

    assert_eq!(show_token(&tokens[1], &idents), "int");
    assert_eq!(show_token(&tokens[2], &idents), "main");
}

#[test]
fn test_numbers() {
    let (tokens, _) = tokenize_str("123 0x1F 3.14 1e10 0.5e-3");
    // Skip StreamBegin/End
    assert_eq!(tokens[1].typ, TokenType::Number);
    assert_eq!(tokens[2].typ, TokenType::Number);
    assert_eq!(tokens[3].typ, TokenType::Number);
    assert_eq!(tokens[4].typ, TokenType::Number);
    assert_eq!(tokens[5].typ, TokenType::Number);

    if let TokenValue::Number(n) = &tokens[1].value {
        assert_eq!(n, "123");
    }
    if let TokenValue::Number(n) = &tokens[2].value {
        assert_eq!(n, "0x1F");
    }
    if let TokenValue::Number(n) = &tokens[3].value {
        assert_eq!(n, "3.14");
    }
    if let TokenValue::Number(n) = &tokens[4].value {
        assert_eq!(n, "1e10");
    }
    if let TokenValue::Number(n) = &tokens[5].value {
        assert_eq!(n, "0.5e-3");
    }
}

#[test]
fn test_strings() {
    let (tokens, _) = tokenize_str(r#""hello" "world""#);
    assert_eq!(tokens[1].typ, TokenType::String);
    assert_eq!(tokens[2].typ, TokenType::String);

    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "hello");
    }
    if let TokenValue::String(s) = &tokens[2].value {
        assert_eq!(s, "world");
    }
}

#[test]
fn test_char_literals() {
    let (tokens, _) = tokenize_str("'a' '\\n' '\\0'");
    assert_eq!(tokens[1].typ, TokenType::Char);
    assert_eq!(tokens[2].typ, TokenType::Char);
    assert_eq!(tokens[3].typ, TokenType::Char);
}

#[test]
fn test_operators() {
    let (tokens, idents) = tokenize_str("+ += ++ - -= -- -> * *= / /= % %= = ==");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        ops,
        vec!["+", "+=", "++", "-", "-=", "--", "->", "*", "*=", "/", "/=", "%", "%=", "=", "=="]
    );
}

#[test]
fn test_comparison_ops() {
    let (tokens, idents) = tokenize_str("< <= > >= == != && || !");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["<", "<=", ">", ">=", "==", "!=", "&&", "||", "!"]);
}

#[test]
fn test_bitwise_ops() {
    let (tokens, idents) = tokenize_str("& &= | |= ^ ^= ~ << >> <<= >>=");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        ops,
        vec!["&", "&=", "|", "|=", "^", "^=", "~", "<<", ">>", "<<=", ">>="]
    );
}

#[test]
fn test_punctuation() {
    let (tokens, idents) = tokenize_str("( ) [ ] { } ; , . ...");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        ops,
        vec!["(", ")", "[", "]", "{", "}", ";", ",", ".", "..."]
    );
}

#[test]
fn test_line_comment() {
    let (tokens, idents) = tokenize_str("a // comment\nb");
    assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_block_comment() {
    let (tokens, idents) = tokenize_str("a /* comment */ b");
    assert_eq!(tokens.len(), 4);
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_line_splice() {
    let (tokens, idents) = tokenize_str("a\\\nb");
    // Line splice joins 'a' and 'b' into one identifier "ab"
    // In C, backslash-newline is deleted, so this becomes "ab"
    assert_eq!(tokens.len(), 3); // StreamBegin, ab, StreamEnd
    assert_eq!(show_token(&tokens[1], &idents), "ab");
}

#[test]
fn test_wide_string() {
    let (tokens, _) = tokenize_str(r#"L"wide""#);
    assert_eq!(tokens[1].typ, TokenType::WideString);
    if let TokenValue::WideString(s) = &tokens[1].value {
        assert_eq!(s, "wide");
    }
}

#[test]
fn test_wide_char() {
    let (tokens, _) = tokenize_str("L'w'");
    assert_eq!(tokens[1].typ, TokenType::WideChar);
}

#[test]
fn test_position_tracking() {
    let (tokens, _) = tokenize_str("a\nb");
    // 'a' is on line 1
    assert_eq!(tokens[1].pos.line, 1);
    // 'b' is on line 2
    assert_eq!(tokens[2].pos.line, 2);
}

#[test]
fn test_newline_flag_first_token() {
    // First token at start of file should have newline=true
    let (tokens, _) = tokenize_str("#define");
    // tokens[0] is STREAM_BEGIN, tokens[1] is the first real token '#'
    assert!(
        tokens[1].pos.newline,
        "First token should have newline=true"
    );
}

#[test]
fn test_newline_flag_after_newline() {
    // Token after newline should have newline=true
    let (tokens, _) = tokenize_str("a\n#define");
    // tokens[0] is STREAM_BEGIN, tokens[1] is 'a', tokens[2] is '#'
    // First token's newline flag isn't constrained - just verify we can access it
    let _ = tokens[1].pos.newline;
    assert!(
        tokens[2].pos.newline,
        "Token after newline should have newline=true"
    );
}

#[test]
fn test_preprocessor_tokens() {
    // C99 6.4.7: the header name is one preprocessing token, delimiters
    // included. Lexed as `<`, `stdio`, `.`, `h`, `>` it had to be
    // reassembled afterwards, and anything the ordinary rules touched --
    // a `//`, an apostrophe -- never survived to be reassembled.
    let (tokens, idents) = tokenize_str("#include <stdio.h>");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(toks, vec!["#", "include", "<stdio.h>"]);
    assert_eq!(tokens[3].typ, TokenType::HeaderName);
}

#[test]
fn test_header_name_is_opaque() {
    for src in [
        "#include <sys//types.h>",
        "#include <it's.h>",
        "#include \"it's.h\"",
        "#include <a\\b.h>",
        "#include <a /* not a comment */ b.h>",
    ] {
        let (tokens, idents) = tokenize_str(src);
        assert_eq!(
            tokens[3].typ,
            TokenType::HeaderName,
            "not lexed as a header name: {src}"
        );
        let spelled = show_token(&tokens[3], &idents);
        assert_eq!(spelled, &src["#include ".len()..], "wrong spelling: {src}");
        // Nothing follows it on the line.
        assert_eq!(
            tokens[4].typ,
            TokenType::StreamEnd,
            "trailing tokens: {src}"
        );
    }

    // `__has_include` in a condition takes one too, and more than one.
    let (tokens, idents) = tokenize_str("#if __has_include(<a//b.h>) && __has_include(<c.h>)");
    let headers: Vec<_> = tokens
        .iter()
        .filter(|t| t.typ == TokenType::HeaderName)
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(headers, vec!["<a//b.h>", "<c.h>"]);
}

#[test]
fn test_header_name_only_where_one_can_appear() {
    // A `<` outside a header-name context is the operator it always was.
    let (tokens, idents) = tokenize_str("if (a<b.c>d) x;");
    assert!(tokens.iter().all(|t| t.typ != TokenType::HeaderName));
    assert_eq!(show_token(&tokens[4], &idents), "<");

    // `#define` is not an include, so `<stdio.h>` there is ordinary.
    let (tokens, _) = tokenize_str("#define H <stdio.h>");
    assert!(tokens.iter().all(|t| t.typ != TokenType::HeaderName));

    // No closing delimiter before end of line: lex it the old way rather
    // than swallow the line, which `#if 0` blocks full of prose need.
    let (tokens, idents) = tokenize_str("#include <unterminated\nint x;");
    assert!(tokens.iter().all(|t| t.typ != TokenType::HeaderName));
    assert_eq!(show_token(&tokens[3], &idents), "<");
}

#[test]
fn test_function_declaration() {
    let (tokens, idents) = tokenize_str("int main(void) { return 0; }");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        toks,
        vec!["int", "main", "(", "void", ")", "{", "return", "0", ";", "}"]
    );
}

// Additional coverage tests for multi-char operators

#[test]
fn test_hashhash_operator() {
    // ## is the preprocessor token paste operator
    let (tokens, idents) = tokenize_str("a ## b");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", "##", "b"]);
}

#[test]
fn test_dotdot_operator() {
    // .. is a two-character operator (range extension)
    let (tokens, idents) = tokenize_str("a .. b");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", "..", "b"]);
}

#[test]
fn test_ternary_operators() {
    // ? and : for ternary expressions
    let (tokens, idents) = tokenize_str("a ? b : c");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", "?", "b", ":", "c"]);
}

#[test]
fn test_all_two_char_operators() {
    // Comprehensive test of ALL 2-char operators
    let (tokens, idents) =
        tokenize_str("+= ++ -= -- -> *= /= %= <= >= == != && &= || |= ^= ## << >> ..");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        ops,
        vec![
            "+=", "++", "-=", "--", "->", "*=", "/=", "%=", "<=", ">=", "==", "!=", "&&", "&=",
            "||", "|=", "^=", "##", "<<", ">>", ".."
        ]
    );
}

#[test]
fn test_all_three_char_operators() {
    // Test all 3-char operators
    let (tokens, idents) = tokenize_str("<<= >>= ...");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["<<=", ">>=", "..."]);
}

#[test]
fn test_three_char_in_context() {
    // 3-char operators in realistic context
    let (tokens, idents) = tokenize_str("x <<= 2; y >>= 1; void f(int a, ...)");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        ops,
        vec![
            "x", "<<=", "2", ";", "y", ">>=", "1", ";", "void", "f", "(", "int", "a", ",", "...",
            ")"
        ]
    );
}

// Multi-line comment tests

#[test]
fn test_multiline_block_comment() {
    let (tokens, idents) = tokenize_str("a /* this is\na multi-line\ncomment */ b");
    assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_block_comment_with_stars() {
    // Comment with * characters inside (common in doc comments)
    let (tokens, idents) = tokenize_str("a /* ** stars ** */ b");
    assert_eq!(tokens.len(), 4);
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_block_comment_with_slashes() {
    // Comment with / characters inside
    let (tokens, idents) = tokenize_str("a /* // not a line comment */ b");
    assert_eq!(tokens.len(), 4);
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_block_comment_asterisk_not_end() {
    // * followed by non-/ should not end comment
    let (tokens, idents) = tokenize_str("a /* x * y */ b");
    assert_eq!(tokens.len(), 4);
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_multiline_comment_position_tracking() {
    // After a multiline comment, position should be correct
    let (tokens, _) = tokenize_str("a\n/* comment\nspanning\nlines */\nb");
    // a is on line 1, b is on line 5
    assert_eq!(tokens[1].pos.line, 1);
    assert_eq!(tokens[2].pos.line, 5);
}

// Additional number format tests

#[test]
fn test_hex_float_numbers() {
    // Hex floats with p/P exponent (C99 feature)
    let (tokens, _) = tokenize_str("0x1p5 0x1.0p10 0xABCp-5 0x1P+3");
    assert_eq!(tokens[1].typ, TokenType::Number);
    assert_eq!(tokens[2].typ, TokenType::Number);
    assert_eq!(tokens[3].typ, TokenType::Number);
    assert_eq!(tokens[4].typ, TokenType::Number);

    if let TokenValue::Number(n) = &tokens[1].value {
        assert_eq!(n, "0x1p5");
    }
    if let TokenValue::Number(n) = &tokens[2].value {
        assert_eq!(n, "0x1.0p10");
    }
    if let TokenValue::Number(n) = &tokens[3].value {
        assert_eq!(n, "0xABCp-5");
    }
    if let TokenValue::Number(n) = &tokens[4].value {
        assert_eq!(n, "0x1P+3");
    }
}

#[test]
fn test_number_suffixes() {
    // Integer suffixes
    let (tokens, _) = tokenize_str("123L 456UL 789LL 0xFFu 42lu");
    for token in tokens.iter().skip(1).take(5) {
        assert_eq!(token.typ, TokenType::Number);
    }
    if let TokenValue::Number(n) = &tokens[1].value {
        assert_eq!(n, "123L");
    }
    if let TokenValue::Number(n) = &tokens[2].value {
        assert_eq!(n, "456UL");
    }
}

#[test]
fn test_float_suffixes() {
    // Float suffixes
    let (tokens, _) = tokenize_str("3.14f 2.71F 1.0l 9.8L");
    for token in tokens.iter().skip(1).take(4) {
        assert_eq!(token.typ, TokenType::Number);
    }
}

#[test]
fn test_dot_starting_number() {
    // Numbers starting with .
    let (tokens, _) = tokenize_str(".5 .123 .0e10");
    assert_eq!(tokens[1].typ, TokenType::Number);
    assert_eq!(tokens[2].typ, TokenType::Number);
    assert_eq!(tokens[3].typ, TokenType::Number);

    if let TokenValue::Number(n) = &tokens[1].value {
        assert_eq!(n, ".5");
    }
}

// Edge cases and tricky sequences

#[test]
fn test_operator_adjacency() {
    // Operators without spaces - maximal munch
    let (tokens, idents) = tokenize_str("a+++b"); // a ++ + b
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", "++", "+", "b"]);
}

#[test]
fn test_operator_adjacency_minus() {
    let (tokens, idents) = tokenize_str("a---b"); // a -- - b
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", "--", "-", "b"]);
}

#[test]
fn test_shift_vs_templates() {
    // >> should be one token (not two > >)
    let (tokens, idents) = tokenize_str("a>>b");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["a", ">>", "b"]);
}

#[test]
fn test_arrow_vs_minus_gt() {
    // -> should be one token
    let (tokens, idents) = tokenize_str("ptr->field");
    let ops: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(ops, vec!["ptr", "->", "field"]);
}

#[test]
fn test_string_with_comment_chars() {
    // String containing /* and */ should not be treated as comment
    let (tokens, _) = tokenize_str(r#""/* not a comment */""#);
    assert_eq!(tokens.len(), 3); // StreamBegin, string, StreamEnd
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "/* not a comment */");
    }
}

#[test]
fn test_char_with_quote() {
    // Character literal with escaped quote
    let (tokens, _) = tokenize_str(r#"'\''"#);
    assert_eq!(tokens[1].typ, TokenType::Char);
    if let TokenValue::Char(s) = &tokens[1].value {
        assert_eq!(s, "\\'");
    }
}

#[test]
fn test_string_with_escaped_quote() {
    let (tokens, _) = tokenize_str(r#""hello \"world\"""#);
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "hello \\\"world\\\"");
    }
}

#[test]
fn test_empty_string() {
    let (tokens, _) = tokenize_str(r#""""#);
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "");
    }
}

#[test]
fn test_empty_char() {
    // Empty char literal (technically invalid C, but lexer should handle)
    let (tokens, _) = tokenize_str("''");
    assert_eq!(tokens[1].typ, TokenType::Char);
    if let TokenValue::Char(s) = &tokens[1].value {
        assert_eq!(s, "");
    }
}

#[test]
fn test_consecutive_comments() {
    let (tokens, idents) = tokenize_str("a /* c1 */ /* c2 */ b");
    assert_eq!(tokens.len(), 4);
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");
}

#[test]
fn test_comment_at_eof() {
    let (tokens, idents) = tokenize_str("a /* comment */");
    assert_eq!(tokens.len(), 3); // StreamBegin, a, StreamEnd
    assert_eq!(show_token(&tokens[1], &idents), "a");
}

#[test]
fn test_line_comment_at_eof() {
    let (tokens, idents) = tokenize_str("a // comment");
    assert_eq!(tokens.len(), 3);
    assert_eq!(show_token(&tokens[1], &idents), "a");
}

// UCN (Universal Character Name) tests - C99 6.4.3

#[test]
fn test_ucn_in_identifier() {
    // Identifier with UCN: caf\u00E9 should become "café"
    let (tokens, idents) = tokenize_str("caf\\u00E9");
    assert_eq!(tokens.len(), 3); // StreamBegin, ident, StreamEnd
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "café");
}

#[test]
fn test_ucn_identifier_start() {
    // Identifier starting with UCN: \u00E9tat -> "état"
    let (tokens, idents) = tokenize_str("\\u00E9tat");
    assert_eq!(tokens.len(), 3);
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "état");
}

#[test]
fn test_ucn_long_form() {
    // Long UCN form, with a code point it is allowed to name.
    //
    // This used to use `\U00000041` and assert the identifier `testAbc`,
    // which C17 6.4.3p2 forbids: a UCN may not name a character below
    // 00A0 other than `$`, `@` and `` ` ``, precisely so it cannot spell
    // an `A` that already has a spelling. gcc rejects that input, and so
    // does c17 now, so the long form is exercised with `\U000000E9`.
    let (tokens, idents) = tokenize_str("test\\U000000E9bc");
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "testébc");
}

#[test]
fn test_ucn_forbidden_characters() {
    // C17 6.4.3p2, in both the short and long forms, and at both ends of
    // the surrogate range -- a surrogate has no `char`, so it used to fail
    // `char::from_u32` and be taken for "not an escape" entirely.
    for src in [
        "test\\u0041bc",
        "test\\U00000041bc",
        "\\u0061bc",
        "test\\u0020bc",
        "test\\ud800bc",
        "test\\udfffbc",
    ] {
        assert!(
            ucn_is_forbidden(match src.split_once("\\u").or(src.split_once("\\U")) {
                Some((_, rest)) => u32::from_str_radix(&rest[..4.min(rest.len())], 16)
                    .unwrap_or_else(|_| u32::from_str_radix(&rest[..8], 16).unwrap()),
                None => unreachable!(),
            }),
            "{src} should name a forbidden character"
        );
    }
    // The three carve-outs, and everything from 00A0 up.
    for val in [0x24, 0x40, 0x60, 0xA0, 0xE9, 0xC5, 0x1F600] {
        assert!(!ucn_is_forbidden(val), "{val:#x} is permitted");
    }
}

#[test]
fn test_ucn_multiple_in_identifier() {
    // Multiple UCNs in one identifier
    let (tokens, idents) = tokenize_str("\\u00E9l\\u00E8ve");
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "élève");
}

#[test]
fn test_ucn_only_identifier() {
    // Identifier consisting only of UCN
    let (tokens, idents) = tokenize_str("\\u03B1"); // Greek alpha
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "α");
}

#[test]
fn test_ucn_lowercase_hex() {
    // UCN with lowercase hex digits
    let (tokens, idents) = tokenize_str("caf\\u00e9");
    assert_eq!(show_token(&tokens[1], &idents), "café");
}

/// Translation phase 2 runs before phase 3, so a splice anywhere in or
/// around a UCN is simply not there by the time the UCN is lexed. The
/// UCN lookahead used to count *bytes* and the consumer to spend them as
/// *characters*, so each splice silently ate that many source characters.
#[test]
fn test_ucn_across_line_splices() {
    // Splice immediately before the UCN: the trailing `zz` must survive.
    let (tokens, idents) = tokenize_str("caf\\\n\\u00e9zz");
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");
    assert_eq!(tokens[2].typ, TokenType::StreamEnd);

    // Splice between the backslash and the `u`.
    let (tokens, idents) = tokenize_str("caf\\\\\nu00e9zz");
    assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");

    // Splice in the middle of the hex digits.
    let (tokens, idents) = tokenize_str("caf\\u00\\\ne9zz");
    assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}zz");

    // Same, for a UCN that *starts* the identifier.
    let (tokens, idents) = tokenize_str("\\u00\\\ne9tat");
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "\u{e9}tat");

    // The long form spans more digits, so it spans more splices.
    let (tokens, idents) = tokenize_str("a\\U000\\\n000\\\ne9b");
    assert_eq!(show_token(&tokens[1], &idents), "a\u{e9}b");
}

/// An unterminated literal ends at the newline it ran into, so the token
/// after it starts a line -- GCC recovers the same way and goes on to
/// process a directive there. Clearing the flag after lexing every token
/// swallowed that newline.
#[test]
fn test_unterminated_literal_yields_the_newline_it_ate() {
    let (tokens, idents) = tokenize_str("char *s = \"abc\n#define ZZZ 9\n");
    let hash = tokens
        .iter()
        .position(|t| show_token(t, &idents) == "#")
        .expect("the `#` must survive as its own token");
    assert!(tokens[hash].pos.newline);
}

/// A non-ASCII byte outside a literal is lexed as its own single-character
/// punctuator, and so is a *byte*, not a character. Rendering one through
/// a Rust `String` UTF-8-encoded it and doubled it, which corrupted every
/// preprocessed `.S` file: a symbol named `café` assembled as `cafÃ©`.
#[test]
fn test_write_token_emits_raw_punctuator_bytes() {
    let (tokens, idents) = tokenize_asm(".globl caf\u{e9}");
    let mut out = Vec::new();
    for t in &tokens[1..tokens.len() - 1] {
        write_token(&mut out, t, &idents);
    }
    // `caf` is an identifier; the two bytes of `é` are punctuators. The
    // stream has to put them back together.
    assert_eq!(out, b".globlcaf\xc3\xa9");

    // tokens_to_source_bytes keeps the spacing and the bytes alike.
    let out = tokens_to_source_bytes(&tokens, &idents);
    assert_eq!(out, b".globl caf\xc3\xa9\n");
}

/// C17 Annex D says which characters an identifier may contain, and D.2
/// which of those may not come first. Checked exhaustively against
/// `gcc -std=c17` when the table was built; these are the boundaries that
/// prove the table is the right one and not merely a plausible one.
#[test]
fn test_identifier_char_annex_d() {
    use IdentPos::{Continue, Initial};

    // D.1 admits these; the code points on either side of each range are
    // what distinguish Annex D from Unicode's XID, which admits far more.
    for ch in [
        '\u{a8}',
        '\u{aa}',
        '\u{b5}',
        '\u{b7}',
        '\u{c0}',
        '\u{d6}',
        '\u{e9}',
        '\u{ff}',
        '\u{100}',
        '\u{3b1}',
        '\u{430}',
        '\u{4e2d}',
        '\u{1f600}',
    ] {
        assert!(identifier_char(ch, Initial), "{ch:?} (U+{:04X})", ch as u32);
        assert!(
            identifier_char(ch, Continue),
            "{ch:?} (U+{:04X})",
            ch as u32
        );
    }

    // Not in D.1 at all, though every one is a perfectly good character
    // and `ucn_is_forbidden` permits it.
    for ch in [
        '\u{a0}', '\u{a1}', '\u{d7}', '\u{f7}', '\u{e000}', '\u{fffe}',
    ] {
        assert!(!identifier_char(ch, Initial), "U+{:04X}", ch as u32);
        assert!(!identifier_char(ch, Continue), "U+{:04X}", ch as u32);
        assert!(
            !ucn_is_forbidden(ch as u32),
            "U+{:04X}: the two rules are different questions",
            ch as u32
        );
    }

    // U+FD3E/U+FD3F are ornate parentheses, which Annex D excludes between
    // the F900-FD3D and FD40-FDCF ranges. GCC's *binary* accepts them
    // anyway, though its own ucnid.tab does not list them and Clang's
    // table does not either; the two ranges either side are accepted by
    // everyone. Following the table is the deliberate choice here.
    assert!(identifier_char('\u{fd3d}', Continue));
    assert!(identifier_char('\u{fd40}', Continue));
    assert!(
        !identifier_char('\u{fd3e}', Continue),
        "Annex D excludes it"
    );
    assert!(
        !identifier_char('\u{fd3f}', Continue),
        "Annex D excludes it"
    );

    // D.2: a combining mark continues an identifier but cannot start one.
    for ch in ['\u{300}', '\u{36f}', '\u{1dc0}', '\u{20d0}', '\u{fe20}'] {
        assert!(!identifier_char(ch, Initial), "U+{:04X}", ch as u32);
        assert!(identifier_char(ch, Continue), "U+{:04X}", ch as u32);
    }

    // The basic source character set answers through the same predicate.
    for ch in ['a', 'Z', '_'] {
        assert!(identifier_char(ch, Initial));
        assert!(identifier_char(ch, Continue));
    }
    assert!(!identifier_char('0', Initial), "a digit cannot start one");
    assert!(identifier_char('0', Continue));
    for ch in ['$', '@', '`', '+', ' '] {
        assert!(!identifier_char(ch, Initial), "{ch:?}");
        assert!(!identifier_char(ch, Continue), "{ch:?}");
    }
}

/// An extended character written directly is the same identifier as the
/// same character written as a UCN, and both stop at a character Annex D
/// does not admit rather than swallowing it.
#[test]
fn test_raw_extended_identifier() {
    let (tokens, idents) = tokenize_str("caf\u{e9}z");
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &idents), "caf\u{e9}z");
    assert_eq!(tokens[2].typ, TokenType::StreamEnd);

    // Same identifier through either spelling.
    let (raw, raw_idents) = tokenize_str("\u{4e2d}\u{6587}");
    let (ucn, ucn_idents) = tokenize_str("\\u4e2d\\u6587");
    assert_eq!(
        show_token(&raw[1], &raw_idents),
        show_token(&ucn[1], &ucn_idents)
    );

    // A combining mark may continue but not start.
    let (tokens, idents) = tokenize_str("a\u{300}");
    assert_eq!(show_token(&tokens[1], &idents), "a\u{300}");
    let (tokens, _) = tokenize_str("\u{300}a");
    assert_ne!(tokens[1].typ, TokenType::Ident);

    // A character outside Annex D ends the identifier and lexes as it did
    // before -- as its own bytes, which `write_token` puts back verbatim.
    let (tokens, idents) = tokenize_str("a\u{d7}b");
    assert_eq!(show_token(&tokens[1], &idents), "a");
    let mut out = Vec::new();
    for t in &tokens[1..tokens.len() - 1] {
        write_token(&mut out, t, &idents);
    }
    assert_eq!(out, "a\u{d7}b".as_bytes());

    // A UCN naming a character no identifier may contain is not one
    // either, though 6.4.3p2 permits the escape itself.
    let (tokens, idents) = tokenize_str("a\\u00d7b");
    assert_eq!(show_token(&tokens[1], &idents), "a");
}

/// A literal payload holds one `char` per source byte, so its spelling has
/// to be written a byte at a time. Formatting one through a Rust `String`
/// UTF-8-encoded each of those chars and doubled every byte >= 0x80.
#[test]
fn test_write_token_emits_source_bytes() {
    let (tokens, idents) = tokenize_str("\"caf\u{e9}\"");
    assert_eq!(tokens[1].typ, TokenType::String);

    let mut out = Vec::new();
    write_token(&mut out, &tokens[1], &idents);
    assert_eq!(out, b"\"caf\xc3\xa9\"");

    // Round-trips through show_token when the bytes are valid UTF-8.
    assert_eq!(show_token(&tokens[1], &idents), "\"caf\u{e9}\"");

    // literal_payload is the inverse: Rust text into payload form.
    assert_eq!(literal_payload("caf\u{e9}"), "caf\u{c3}\u{a9}");
    assert_eq!(
        payload_bytes(&literal_payload("caf\u{e9}")).collect::<Vec<_>>(),
        b"caf\xc3\xa9"
    );

    // payload_text reads one back out, for the consumers that want Rust
    // text: a header name to open, an `__asm__` symbol, a message to
    // print. Reading a payload as if it were already text gave mojibake.
    assert_eq!(payload_text(&literal_payload("caf\u{e9}")), "caf\u{e9}");
    assert_eq!(payload_text("caf\u{c3}\u{a9}"), "caf\u{e9}");
    for text in ["", "plain", "caf\u{e9}", "\u{2603} \u{1f600}"] {
        assert_eq!(
            payload_text(&literal_payload(text)),
            text,
            "round trip {text:?}"
        );
    }

    // And the payload of a literal the lexer produced decodes to what the
    // source said, which is the property the two conventions must share.
    let (tokens, _) = tokenize_str("\"caf\u{e9}\"");
    if let TokenValue::String(payload) = &tokens[1].value {
        assert_eq!(payload_text(payload), "caf\u{e9}");
    } else {
        panic!("not a string literal");
    }
}

#[test]
fn test_column_saturates_on_very_long_line() {
    let mut src = " ".repeat(70000);
    src.push('x');
    let (tokens, idents) = tokenize_str(&src);
    assert_eq!(show_token(&tokens[1], &idents), "x");
    assert_eq!(tokens[1].pos.col, u16::MAX);
    assert_eq!(tokens[1].pos.line, 1);

    // Tabs advance to the next multiple of eight, which must saturate too.
    let mut src = "\t".repeat(70000);
    src.push('x');
    let (tokens, idents) = tokenize_str(&src);
    assert_eq!(show_token(&tokens[1], &idents), "x");
    assert!(tokens[1].pos.col >= u16::MAX - 8);
}

/// A `%:` whose following `%` does not complete the `%:%:` digraph used to
/// be consumed and then rewound by hand. The rewind restored `offset` and
/// `col` but not `line`, so a splice between the two halves was counted
/// once on the way in and again on the way out.
#[test]
fn test_digraph_hash_not_hashhash_keeps_line_count() {
    let (tokens, idents) = tokenize_str("%:\\\n% x\ny");
    let spelled: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    // `%:` keeps its own spelling (6.4.6p3) while meaning `#`.
    assert_eq!(spelled, vec!["%:", "%", "x", "y"]);
    assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'#' as u32));
    // Phase 2 deletes the splice but the physical lines still count, so
    // `x` is on line 2 and `y` on line 3 -- each crossed exactly once.
    assert_eq!(tokens[3].pos.line, 2);
    assert_eq!(tokens[4].pos.line, 3);
}

/// With phase 2 off (a `.i` operand) a backslash-newline is text, not a
/// joint, so none of the above applies and the UCN does not form.
#[test]
fn test_ucn_splice_disabled() {
    let mut strings = StringTable::new();
    let mut tokenizer = Tokenizer::new(b"caf\\u00\\\ne9", 0, &mut strings).without_splicing();
    let tokens = tokenizer.tokenize();
    assert_eq!(tokens[1].typ, TokenType::Ident);
    assert_eq!(show_token(&tokens[1], &strings), "caf");
}

// Diagnostic warning tests

#[test]
fn test_unterminated_string() {
    // Unterminated string should still produce a token (warning emitted)
    let (tokens, _) = tokenize_str("\"hello");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "hello");
    }
}

#[test]
fn test_unterminated_char() {
    // Unterminated char should still produce a token (warning emitted)
    let (tokens, _) = tokenize_str("'a");
    assert_eq!(tokens[1].typ, TokenType::Char);
    if let TokenValue::Char(s) = &tokens[1].value {
        assert_eq!(s, "a");
    }
}

#[test]
fn test_newline_in_string() {
    // Newline terminates string literal (warning emitted)
    let (tokens, _) = tokenize_str("\"hello\nworld\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "hello");
    }
    // 'world"' becomes identifier 'world' and unterminated string
    assert_eq!(tokens[2].typ, TokenType::Ident);
}

#[test]
fn test_unterminated_block_comment() {
    // Unterminated block comment (warning emitted)
    let (tokens, idents) = tokenize_str("a /* unterminated");
    assert_eq!(tokens.len(), 3); // StreamBegin, a, StreamEnd
    assert_eq!(show_token(&tokens[1], &idents), "a");
}

#[test]
fn test_hex_escape_no_digits() {
    // \x without hex digits should warn
    let (tokens, _) = tokenize_str("\"\\xg\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "\\xg"); // Raw escape preserved
    }
}

#[test]
fn test_hex_escape_at_end() {
    // \x at end of string should warn
    let (tokens, _) = tokenize_str("\"\\x\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "\\x");
    }
}

#[test]
fn test_hex_escape_valid() {
    // Valid \x escape (no warning)
    let (tokens, _) = tokenize_str("\"\\x41\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "\\x41"); // Raw escape preserved
    }
}

#[test]
fn test_octal_escape_preserved() {
    // Octal escapes should be preserved as raw
    let (tokens, _) = tokenize_str("\"\\0\\377\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "\\0\\377");
    }
}

#[test]
fn test_standard_escapes_preserved() {
    // Standard escapes should be preserved as raw
    let (tokens, _) = tokenize_str("\"\\n\\t\\r\\\\\"");
    assert_eq!(tokens[1].typ, TokenType::String);
    if let TokenValue::String(s) = &tokens[1].value {
        assert_eq!(s, "\\n\\t\\r\\\\");
    }
}

#[test]
fn test_token_no_expand_initially_none() {
    let token = Token::new(TokenType::Ident, Position::default());
    assert!(token.no_expand.is_none());
    assert!(!token.is_no_expand("FOO"));
}

#[test]
fn test_token_mark_no_expand() {
    let mut token = Token::new(TokenType::Ident, Position::default());

    // Mark a macro as no-expand
    token.mark_no_expand("FOO");
    assert!(token.is_no_expand("FOO"));
    assert!(!token.is_no_expand("BAR"));

    // Can mark multiple macros
    token.mark_no_expand("BAR");
    assert!(token.is_no_expand("FOO"));
    assert!(token.is_no_expand("BAR"));
    assert!(!token.is_no_expand("BAZ"));
}

#[test]
fn test_token_with_value_no_expand_none() {
    let token = Token::with_value(
        TokenType::Number,
        Position::default(),
        TokenValue::Number("42".to_string()),
    );
    assert!(token.no_expand.is_none());
}

#[test]
fn test_multiline_comment_newline_flag() {
    // After a multiline comment, the next token should NOT have
    // newline=true just because the comment spanned lines.
    // The comment fix resets the newline flag.
    let (tokens, idents) = tokenize_str("a /* multi\nline\ncomment */ b");
    assert_eq!(tokens.len(), 4); // StreamBegin, a, b, StreamEnd

    // 'a' and 'b' should both be identifiers
    assert_eq!(show_token(&tokens[1], &idents), "a");
    assert_eq!(show_token(&tokens[2], &idents), "b");

    // 'b' should NOT have newline=true (it follows comment on same logical line)
    // The comment was on the same line as 'a', so 'b' continues that line
    assert!(
        !tokens[2].pos.newline,
        "token after multiline comment should not have newline flag"
    );
}

// Assembly mode tests

fn tokenize_asm(input: &str) -> (Vec<Token>, StringTable) {
    let mut strings = StringTable::new();
    let mut tokenizer =
        Tokenizer::new_with_mode(input.as_bytes(), 0, &mut strings, LexerMode::Assembly);
    let tokens = tokenizer.tokenize();
    (tokens, strings)
}

#[test]
fn test_asm_semicolon_not_comment() {
    // In assembly mode, `;` is NOT treated as comment (it's a statement
    // separator in GAS/AT&T syntax). Comment handling is left to the assembler.
    let (tokens, idents) = tokenize_asm("mov eax, ebx ; move register");
    // Should get full line tokenized, including ; and subsequent identifiers
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        toks,
        vec!["mov", "eax", ",", "ebx", ";", "move", "register"]
    );
}

#[test]
fn test_asm_comments_are_stripped() {
    // GCC's assembler-with-cpp strips `//` and `/* */` from assembly just
    // as it does from C, so c17 does too.
    let (tokens, idents) = tokenize_asm("a // b");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(toks, vec!["a"]);

    let (tokens, idents) = tokenize_asm("a /* b */ c");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(toks, vec!["a", "c"]);
}

/// An apostrophe in assembly is prose or a GNU as character constant, not
/// the start of a C literal. Lexing it as one swallowed the rest of the
/// line, so a `.S` file whose comment said "don't" assembled to something
/// else entirely -- or failed outright.
#[test]
fn test_asm_apostrophe_is_not_a_literal() {
    let (tokens, idents) = tokenize_asm("# don't panic\nmovl $7, %eax");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        toks,
        vec!["#", "don", "'", "t", "panic", "movl", "$", "7", ",", "%", "eax"]
    );

    // GNU as writes an unterminated character constant `'a`, and a
    // terminated one `'b'`; both are just punctuation plus identifiers.
    let (tokens, idents) = tokenize_asm(".byte 'a\n.byte 'b'");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(
        toks,
        vec![".", "byte", "'", "a", ".", "byte", "'", "b", "'"]
    );

    // `"` still opens a string, so an apostrophe inside one is content.
    let (tokens, idents) = tokenize_asm(".ascii \"it's fine\"");
    assert_eq!(tokens[3].typ, TokenType::String);
    assert_eq!(show_token(&tokens[3], &idents), "\"it's fine\"");
}

#[test]
fn test_asm_semicolon_at_start_of_line() {
    // Semicolon comment at start of line
    let (tokens, _) = tokenize_asm("; This is a full line comment\nmov eax, 1");
    // First line should be completely ignored
    assert!(tokens.len() >= 4); // StreamBegin, mov, eax, ..., StreamEnd
}

#[test]
fn test_asm_mode_preserves_preprocessor_directives() {
    // Assembly preprocessing should still handle # directives
    let (tokens, idents) = tokenize_asm("#define FOO 1\nmov eax, FOO");
    // Should tokenize the # directive
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert!(toks.contains(&"#".to_string()));
    assert!(toks.contains(&"define".to_string()));
}

#[test]
fn test_c_mode_semicolon_not_comment() {
    // In C mode, `;` is a statement terminator, not a comment
    let (tokens, idents) = tokenize_str("int x; int y");
    let toks: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(toks, vec!["int", "x", ";", "int", "y"]);
}

// tokens_to_source_bytes tests

fn source_text(input: &str) -> String {
    let (tokens, strings) = tokenize_str(input);
    String::from_utf8(tokens_to_source_bytes(&tokens, &strings)).expect("not UTF-8")
}

#[test]
fn test_tokens_to_source_bytes_simple() {
    // Note: semicolon doesn't have whitespace flag set, so no space before it
    assert_eq!(source_text("int x = 42;").trim(), "int x = 42;");
}

#[test]
fn test_tokens_to_source_bytes_multiline() {
    let text = source_text("int x;\nint y;");
    // Should preserve the newline between statements
    assert!(text.contains('\n'));
    assert!(text.contains("int"));
}

#[test]
fn test_tokens_to_source_bytes_ends_with_newline() {
    assert!(source_text("x").ends_with('\n'));
}

/// The reason this path deals in bytes: a literal payload is one `char`
/// per source byte, and rendering it as a Rust string doubles every byte
/// of 0x80 or more.
#[test]
fn test_tokens_to_source_bytes_keeps_literal_bytes() {
    let (tokens, strings) = tokenize_str(".ascii \"caf\u{e9}\"");
    let out = tokens_to_source_bytes(&tokens, &strings);
    assert_eq!(out, b".ascii \"caf\xc3\xa9\"\n");
}

// C99 6.4.6 Digraph tests

#[test]
fn test_digraph_brackets() {
    // <: and :> are digraphs for [ and ]
    let (tokens, _) = tokenize_str("<:0:>");
    // StreamBegin, [, 0, ], StreamEnd
    assert_eq!(tokens.len(), 5);
    assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'[' as u32));
    assert!(matches!(&tokens[2].value, TokenValue::Number(n) if n == "0"));
    assert!(matches!(&tokens[3].value, TokenValue::Special(c) if *c == b']' as u32));
}

#[test]
fn test_digraph_braces() {
    // <% and %> are digraphs for { and }
    let (tokens, _) = tokenize_str("<% %>");
    // StreamBegin, {, }, StreamEnd
    assert_eq!(tokens.len(), 4);
    assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'{' as u32));
    assert!(matches!(&tokens[2].value, TokenValue::Special(c) if *c == b'}' as u32));
}

#[test]
fn test_digraph_hash() {
    // %: is digraph for #
    let (tokens, _) = tokenize_str("%: define");
    // StreamBegin, #, define, StreamEnd
    assert_eq!(tokens.len(), 4);
    assert!(matches!(&tokens[1].value, TokenValue::Special(c) if *c == b'#' as u32));
}

#[test]
fn test_digraph_hashhash() {
    // %:%: is digraph for ##
    let (tokens, _) = tokenize_str("%:%:");
    // StreamBegin, ##, StreamEnd
    assert_eq!(tokens.len(), 3);
    assert!(
        matches!(&tokens[1].value, TokenValue::Special(c) if *c == SpecialToken::HashHash as u32)
    );
}

/// 6.4.6p3: a digraph behaves as its primary token "except for their
/// spelling", and 6.10.3.2p2 makes `#` reproduce that spelling. The value
/// is therefore the primary token's and the spelling its own.
#[test]
fn test_digraph_keeps_its_own_spelling() {
    for (src, primary) in [
        ("<:", b'[' as u32),
        (":>", b']' as u32),
        ("<%", b'{' as u32),
        ("%>", b'}' as u32),
        ("%:", b'#' as u32),
        ("%:%:", SpecialToken::HashHash as u32),
    ] {
        let (tokens, idents) = tokenize_str(src);
        assert!(
            matches!(&tokens[1].value, TokenValue::Special(c) if *c == primary),
            "{src} must mean its primary token"
        );
        assert_eq!(
            show_token(&tokens[1], &idents),
            src,
            "{src} lost its spelling"
        );
    }

    // The primary tokens keep spelling themselves.
    let (tokens, idents) = tokenize_str("[ ] { } # ##");
    let spelled: Vec<_> = tokens[1..tokens.len() - 1]
        .iter()
        .map(|t| show_token(t, &idents))
        .collect();
    assert_eq!(spelled, vec!["[", "]", "{", "}", "#", "##"]);
}

// Character classification table tests

#[test]
fn test_char_table_digits() {
    for c in b'0'..=b'9' {
        let cl = char_class(c);
        assert_eq!(cl & DIGIT, DIGIT, "digit {}", c as char);
        assert_eq!(cl & HEX, HEX, "digit hex {}", c as char);
        assert_eq!(cl & LETTER, 0, "digit not letter {}", c as char);
    }
}

#[test]
fn test_char_table_hex_letters() {
    for c in *b"ABCDFabcdf" {
        let cl = char_class(c);
        assert_eq!(cl & LETTER, LETTER, "hex letter {}", c as char);
        assert_eq!(cl & HEX, HEX, "hex flag {}", c as char);
    }
}

#[test]
fn test_char_table_exp_letters() {
    for c in *b"EePp" {
        let cl = char_class(c);
        assert_eq!(cl & EXP, EXP, "exp {}", c as char);
        assert_eq!(cl & LETTER, LETTER, "exp letter {}", c as char);
    }
    // E and e are also hex
    assert_ne!(char_class(b'E') & HEX, 0);
    assert_ne!(char_class(b'e') & HEX, 0);
    // P and p are NOT hex
    assert_eq!(char_class(b'P') & HEX, 0);
    assert_eq!(char_class(b'p') & HEX, 0);
}

#[test]
fn test_char_table_plain_letters() {
    // Non-hex, non-exp uppercase
    for c in b'G'..=b'O' {
        let cl = char_class(c);
        assert_eq!(cl, LETTER, "plain upper {}", c as char);
    }
    for c in b'Q'..=b'Z' {
        let cl = char_class(c);
        assert_eq!(cl, LETTER, "plain upper {}", c as char);
    }
    // Non-hex, non-exp lowercase
    for c in b'g'..=b'o' {
        let cl = char_class(c);
        assert_eq!(cl, LETTER, "plain lower {}", c as char);
    }
    for c in b'q'..=b'z' {
        let cl = char_class(c);
        assert_eq!(cl, LETTER, "plain lower {}", c as char);
    }
    assert_eq!(char_class(b'_'), LETTER);
}

#[test]
fn test_char_table_dot() {
    let cl = char_class(b'.');
    assert_ne!(cl & DOT, 0);
    assert_ne!(cl & VALID_SECOND, 0);
}

#[test]
fn test_char_table_valid_second() {
    for c in *b"=+-><&|#" {
        assert_ne!(
            char_class(c) & VALID_SECOND,
            0,
            "valid_second {}",
            c as char
        );
    }
}

#[test]
fn test_char_table_quote() {
    assert_ne!(char_class(b'\'') & QUOTE, 0);
    assert_ne!(char_class(b'"') & QUOTE, 0);
}

#[test]
fn test_char_table_comment() {
    assert_ne!(char_class(b'/') & COMMENT, 0);
}

#[test]
fn test_char_table_zero_for_others() {
    // Control characters, whitespace, misc punctuation not in the table
    for c in [0u8, b' ', b'\t', b'\n', b'@', b'$', b'`', b'~', 0x80, 0xFF] {
        assert_eq!(char_class(c), 0, "zero for byte {:#x}", c);
    }
}

/// `U+FEFF` is a legal identifier character under Annex D.1, so the tables are
/// right to admit it and the byte order mark cannot be fixed by rejecting it.
/// It comes off the buffer instead.
#[test]
fn test_strip_bom() {
    assert_eq!(strip_bom(b"\xEF\xBB\xBF#define X 1"), b"#define X 1");
    // Only at the very start, and only one.
    assert_eq!(strip_bom(b"int v;"), b"int v;");
    assert_eq!(
        strip_bom(b"\xEF\xBB\xBF\xEF\xBB\xBFx"),
        b"\xEF\xBB\xBFx".as_slice()
    );
    assert_eq!(strip_bom(b"a\xEF\xBB\xBFb"), b"a\xEF\xBB\xBFb".as_slice());
    assert_eq!(strip_bom(b""), b"");
    // A prefix of the mark is not the mark.
    assert_eq!(strip_bom(b"\xEF\xBB"), b"\xEF\xBB".as_slice());
}
