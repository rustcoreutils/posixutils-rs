// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parser::lex::lex;
use rowan::ast::AstNode;
use std::str::FromStr;

use super::SyntaxKind::*;

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Parse(ParseError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Error::Io(e) => write!(f, "IO error: {}", e),
            Error::Parse(e) => write!(f, "Parse error: {}", e),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub Vec<String>);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for err in &self.0 {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parse(e)
    }
}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::GreenNode;

use super::SyntaxKind;
/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;

/// The parse results are stored as a "green tree".
/// We'll discuss working with the results later
#[derive(Debug)]
pub struct Parse {
    green_node: GreenNode,
    #[allow(unused)]
    pub errors: Vec<String>,
}

#[derive(Clone)]
pub struct Parsed(GreenNode);

impl Parsed {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.0.clone())
    }

    pub fn root(&self) -> Makefile {
        Makefile::cast(self.syntax()).unwrap()
    }
}

pub fn parse(text: &str) -> Result<Parsed, ParseError> {
    struct Parser {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, String)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder<'static>,
        /// the list of syntax errors we've accumulated
        /// so far.
        errors: Vec<String>,
    }

    impl Parser {
        fn error(&mut self, msg: String) {
            self.builder.start_node(ERROR.into());
            if self.current().is_some() {
                self.bump();
            }
            self.errors.push(msg);
            self.builder.finish_node();
        }

        fn parse_expr(&mut self) {
            self.builder.start_node(EXPR.into());
            loop {
                match self.current() {
                    Some(NEWLINE) | None => {
                        break;
                    }
                    Some(_t) => {
                        self.bump();
                    }
                }
            }
            self.builder.finish_node();
        }

        fn parse_recipe_line(&mut self) {
            self.builder.start_node(RECIPE.into());
            self.expect(INDENT);
            self.expect(TEXT);
            self.try_expect(NEWLINE);
            self.builder.finish_node();
        }

        fn parse_include(&mut self) {
            self.builder.start_node(RULE.into());
            self.expect(IDENTIFIER);
            self.skip_ws();
            if self.tokens.last() == Some(&(IDENTIFIER, "include".to_string())) {
                self.expect(IDENTIFIER);
                self.skip_ws();
            }
            self.try_expect(NEWLINE);
            self.builder.token(IDENTIFIER.into(), "variables.mk");
            dbg!(&self.builder);
            self.builder.finish_node();
        }

        fn parse_macro_defintion(&mut self) {
            self.builder.start_node(MACRODEF.into());
            self.try_expect(EXPORT);
            self.expect(IDENTIFIER);
            self.expect(EQUALS);
            self.parse_expr();
            self.builder.finish_node();
        }

        fn parse_rule(&mut self) {
            self.builder.start_node(RULE.into());
            self.skip_ws();
            self.try_expect(EXPORT);
            self.skip_ws();
            let is_pattern = self.try_expect(PERCENT);
            if !is_pattern {
                self.expect(IDENTIFIER);
            }
            self.skip_ws();
            if self.tokens.pop() == Some((COLON, ":".to_string())) {
                self.builder.token(COLON.into(), ":");
            } else {
                self.error("expected ':'".into());
            }
            self.skip_ws();
            self.parse_expr();
            self.try_expect(NEWLINE);
            loop {
                match self.current() {
                    Some(INDENT) => {
                        self.parse_recipe_line();
                    }
                    Some(NEWLINE) => {
                        self.bump();
                        break;
                    }
                    _ => {
                        break;
                    }
                }
            }
            self.builder.finish_node();
        }

        fn parse(mut self) -> Parse {
            self.builder.start_node(ROOT.into());
            loop {
                match self
                    .find(|&&(k, _)| k == COLON || k == NEWLINE || k == INCLUDE || k == EQUALS)
                {
                    Some((EQUALS, "=")) => self.parse_macro_defintion(),
                    Some((COLON, ":")) => {
                        self.parse_rule();
                    }
                    Some((INCLUDE, "include")) => {
                        dbg!(&self.tokens);
                        self.parse_include();
                    }
                    Some((NEWLINE, _)) => {
                        self.bump();
                    }
                    Some(_) | None => {
                        self.error(" *** No targets. Stop.".to_string());
                        if self.current().is_some() {
                            self.bump();
                        }
                    }
                }

                if self.current().is_none() {
                    break;
                }
            }
            // Close the root node.
            self.builder.finish_node();

            // Turn the builder into a GreenNode
            Parse {
                green_node: self.builder.finish(),
                errors: self.errors,
            }
        }
        /// Advance one token, adding it to the current branch of the tree builder.
        fn bump(&mut self) {
            let (kind, text) = self.tokens.pop().unwrap();
            self.builder.token(kind.into(), text.as_str());
        }
        /// Peek at the first unprocessed token
        fn current(&self) -> Option<SyntaxKind> {
            self.tokens.last().map(|(kind, _)| *kind)
        }

        fn find(
            &self,
            finder: impl FnMut(&&(SyntaxKind, String)) -> bool,
        ) -> Option<(SyntaxKind, &str)> {
            self.tokens
                .iter()
                .rev()
                .find(finder)
                .map(|(kind, text)| (*kind, text.as_str()))
        }

        fn expect(&mut self, expected: SyntaxKind) {
            if self.current() != Some(expected) {
                self.error(format!("expected {:?}, got {:?}", expected, self.current()));
            } else {
                self.bump();
            }
        }

        fn try_expect(&mut self, expected: SyntaxKind) -> bool {
            if self.current() != Some(expected) {
                false
            } else {
                self.bump();
                true
            }
        }

        fn skip_ws(&mut self) {
            while self.current() == Some(WHITESPACE) {
                self.bump()
            }
        }
    }

    let mut tokens = lex(text);

    tokens.reverse();
    let result = Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
    }
    .parse();

    if !result.errors.is_empty() {
        Err(ParseError(result.errors))
    } else {
        Ok(Parsed(result.green_node))
    }
}

/// To work with the parse results we need a view into the
/// green tree - the Syntax tree.
/// It is also immutable, like a GreenNode,
/// but it contains parent pointers, offsets, and
/// has identity semantics.

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn root(&self) -> Makefile {
        Makefile::cast(self.syntax()).unwrap()
    }
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxNode);

        impl AstNode for $ast {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self(syntax))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }

        impl core::fmt::Display for $ast {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
                write!(f, "{}", self.0.text())
            }
        }
    };
}

ast_node!(Macro, MACRO);
ast_node!(MacroDef, MACRODEF);
ast_node!(Makefile, ROOT);
ast_node!(Rule, RULE);
ast_node!(Identifier, IDENTIFIER);

impl Macro {}
impl MacroDef {}

impl MacroDef {
    pub fn name(&self) -> Option<String> {
        self.syntax().children_with_tokens().find_map(|it| {
            it.as_token().and_then(|it| {
                if it.kind() == IDENTIFIER && it.text() != "export" {
                    Some(it.text().to_string())
                } else {
                    None
                }
            })
        })
    }

    pub fn raw_value(&self) -> Option<String> {
        self.syntax()
            .children()
            .find(|it| it.kind() == EXPR)
            .map(|it| it.text().to_string())
    }
}

impl Makefile {
    pub fn new() -> Makefile {
        let mut builder = GreenNodeBuilder::new();

        builder.start_node(ROOT.into());
        builder.finish_node();

        let syntax = SyntaxNode::new_root(builder.finish());
        Makefile(syntax.clone_for_update())
    }

    /// Read a changelog file from a reader
    pub fn read<R: std::io::Read>(mut r: R) -> Result<Makefile, Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;
        Ok(buf.parse()?)
    }

    pub fn read_relaxed<R: std::io::Read>(mut r: R) -> Result<Makefile, Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;

        let parsed = parse(&buf)?;
        Ok(parsed.root())
    }

    pub fn rules(&self) -> impl Iterator<Item = Rule> {
        self.syntax().children().filter_map(Rule::cast)
    }

    pub fn macros(&self) -> impl Iterator<Item = MacroDef> {
        self.syntax().children().filter_map(MacroDef::cast)
    }

    pub fn rules_by_target<'a>(&'a self, target: &'a str) -> impl Iterator<Item = Rule> + 'a {
        self.rules()
            .filter(move |rule| rule.targets().any(|t| t == target))
    }

    pub fn variable_definitions(&self) -> impl Iterator<Item = MacroDef> {
        self.syntax().children().filter_map(MacroDef::cast)
    }

    pub fn add_rule(&mut self, target: &str) -> Rule {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(RULE.into());
        builder.token(IDENTIFIER.into(), target);
        builder.token(COLON.into(), ":");
        builder.token(NEWLINE.into(), "\n");
        builder.finish_node();

        let syntax = SyntaxNode::new_root(builder.finish()).clone_for_update();
        let pos = self.0.children_with_tokens().count();
        self.0
            .splice_children(pos..pos, vec![syntax.clone().into()]);
        Rule(syntax)
    }
}

impl Rule {
    pub fn targets(&self) -> impl Iterator<Item = String> {
        self.syntax()
            .children_with_tokens()
            .take_while(|it| it.as_token().map_or(true, |t| t.kind() != COLON))
            .filter_map(|it| it.as_token().map(|t| t.text().to_string()))
    }

    pub fn prerequisites(&self) -> impl Iterator<Item = String> {
        self.syntax()
            .children()
            .find(|it| it.kind() == EXPR)
            .into_iter()
            .flat_map(|it| {
                it.children_with_tokens().filter_map(|it| {
                    it.as_token().and_then(|t| {
                        if t.kind() == IDENTIFIER {
                            Some(t.text().to_string())
                        } else {
                            None
                        }
                    })
                })
            })
    }

    pub fn recipes(&self) -> impl Iterator<Item = String> {
        self.syntax()
            .children()
            .filter(|it| it.kind() == RECIPE)
            .flat_map(|it| {
                it.children_with_tokens().filter_map(|it| {
                    it.as_token().and_then(|t| {
                        if t.kind() == TEXT {
                            Some(t.text().to_string())
                        } else {
                            None
                        }
                    })
                })
            })
    }

    pub fn replace_command(&self, i: usize, line: &str) {
        // Find the RECIPE with index i, then replace the line in it
        let index = self
            .syntax()
            .children()
            .filter(|it| it.kind() == RECIPE)
            .nth(i)
            .expect("index out of bounds")
            .index();

        let mut builder = GreenNodeBuilder::new();
        builder.start_node(RECIPE.into());
        builder.token(INDENT.into(), "\t");
        builder.token(TEXT.into(), line);
        builder.token(NEWLINE.into(), "\n");
        builder.finish_node();

        let syntax = SyntaxNode::new_root(builder.finish()).clone_for_update();
        self.0
            .splice_children(index..index + 1, vec![syntax.into()]);
    }

    pub fn push_command(&self, line: &str) {
        // Find the latest RECIPE entry, then append the new line after it.
        let index = self
            .0
            .children_with_tokens()
            .filter(|it| it.kind() == RECIPE)
            .last();

        let index = index.map_or_else(
            || self.0.children_with_tokens().count(),
            |it| it.index() + 1,
        );

        let mut builder = GreenNodeBuilder::new();
        builder.start_node(RECIPE.into());
        builder.token(INDENT.into(), "\t");
        builder.token(TEXT.into(), line);
        builder.token(NEWLINE.into(), "\n");
        builder.finish_node();
        let syntax = SyntaxNode::new_root(builder.finish()).clone_for_update();

        self.0.splice_children(index..index, vec![syntax.into()]);
    }
}

impl Default for Makefile {
    fn default() -> Self {
        Self::new()
    }
}

impl FromStr for Makefile {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s).map(|node| node.root())
    }
}
