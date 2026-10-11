//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unit tests for the `-E` writer, driven by hand-made token streams so that
// each marker rule is pinned on its own.
//

use super::*;
use crate::token::lexer::Tokenizer;

/// A stream registered under `name`, whose tokens are the lexed `text`
/// without its stream markers.
fn lexed(name: &str, text: &str, strings: &mut StringTable) -> (u16, Vec<Token>) {
    let stream = diag::init_stream(name);
    let tokens = Tokenizer::new(text.as_bytes(), stream, strings)
        .tokenize()
        .into_iter()
        .filter(|t| !matches!(t.typ, TokenType::StreamBegin | TokenType::StreamEnd))
        .collect();
    (stream, tokens)
}

fn change(change: FileChange, pos: Position) -> Token {
    Token::with_value(TokenType::FileChange, pos, TokenValue::FileChange(change))
}

fn written(tokens: &[Token], strings: &StringTable, markers: bool) -> String {
    let mut out = Vec::new();
    write_preprocessed(
        &mut out,
        tokens,
        strings,
        "m.c",
        InitialDefinitions::default(),
        Layout {
            markers,
            language: LexerMode::C,
            preprocessed: false,
        },
    )
    .unwrap();
    String::from_utf8(out).unwrap()
}

/// The source, an include of a header with no text and one with text on
/// its second line, and the source again: gcc's markers, one for one -- the
/// pseudo-files at line 0, entry with flag 1 at line 1 whatever the first
/// token's line, the return with flag 2 to the line after the directive,
/// and blank lines for a short gap.
#[test]
fn test_markers_follow_inclusion_not_tokens() {
    let mut strings = StringTable::new();
    let (main, src) = lexed("m.c", "int a;\n\n\n\n\nint b;\n", &mut strings);
    let (empty, _) = lexed("e.h", "", &mut strings);
    let (header, text) = lexed("h.h", "\nint h;\n", &mut strings);
    let mut tokens = vec![change(FileChange::Rename, Position::new(main, 1, 1))];
    tokens.extend(src[..3].iter().cloned());
    tokens.push(change(
        FileChange::Enter { stream: empty },
        Position::new(main, 2, 1),
    ));
    tokens.push(change(FileChange::Return, Position::new(main, 3, 1)));
    tokens.push(change(
        FileChange::Enter { stream: header },
        Position::new(main, 3, 1),
    ));
    tokens.extend(text);
    tokens.push(change(FileChange::Return, Position::new(main, 4, 1)));
    tokens.extend(src[3..].iter().cloned());
    assert_eq!(
        written(&tokens, &strings, true),
        "# 0 \"m.c\"\n# 0 \"<built-in>\"\n# 0 \"<command-line>\"\n# 1 \"m.c\"\n\
         int a;\n# 1 \"e.h\" 1\n# 3 \"m.c\" 2\n# 1 \"h.h\" 1\n\nint h;\n\
         # 4 \"m.c\" 2\n\n\nint b;\n"
    );
    // `-P`: the text alone, gaps closed up.
    assert_eq!(
        written(&tokens, &strings, false),
        "int a;\nint h;\nint b;\n"
    );
}

/// A gap of eight lines or more is a marker, not blank lines, and a
/// system header's markers carry gcc's `3 4`; where text from a system
/// header meets other text, the token is marked again on a line of its
/// own, which leaves the blank line gcc leaves.
#[test]
fn test_long_gap_and_system_header_markers() {
    let mut strings = StringTable::new();
    let (main, src) = lexed("s.c", "int a;\n", &mut strings);
    let (sys, text) = lexed("sys.h", "int s;\n\n\n\n\n\n\n\n\nint t;\n", &mut strings);
    diag::set_stream_system(sys, true);
    let mut tokens = vec![
        change(FileChange::Rename, Position::new(main, 1, 1)),
        change(FileChange::Enter { stream: sys }, Position::new(main, 1, 1)),
    ];
    tokens.extend(text);
    tokens.push(change(FileChange::Return, Position::new(main, 2, 1)));
    tokens.extend(src.into_iter().map(|mut t| {
        t.pos.line = 2;
        t
    }));
    let out = written(&tokens, &strings, true);
    let expected = "# 1 \"s.c\"\n# 1 \"sys.h\" 1 3 4\n\n\
                    # 1 \"sys.h\" 3 4\nint s;\n# 10 \"sys.h\" 3 4\nint t;\n\
                    # 2 \"s.c\" 2\n\n# 2 \"s.c\"\nint a;\n";
    assert!(out.ends_with(expected), "{out}");
}
