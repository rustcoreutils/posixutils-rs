//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Parser for yacc grammar files

use crate::error::YaccError;
use crate::lexer::{PositionedToken, Token};

/// Associativity of operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    NonAssoc,
}

/// A symbol in the grammar (terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    /// Non-terminal
    NonTerminal(String),
    /// Character literal terminal
    CharLiteral(char),
    /// The special error token
    Error,
}

/// Token declaration (%token, %left, %right, %nonassoc)
#[derive(Debug, Clone)]
pub struct TokenDecl {
    pub name: String,
    pub tag: Option<String>,
    pub number: Option<i32>,
    pub precedence: Option<usize>,
    pub associativity: Option<Associativity>,
}

/// Type declaration (%type)
#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub tag: String,
    pub symbols: Vec<String>,
}

/// A production rule
#[derive(Debug, Clone)]
pub struct Rule {
    pub lhs: String,
    pub rhs: Vec<RhsElement>,
    pub action: Option<String>,
    pub prec: Option<String>,
    pub line: usize,
}

/// An element on the right-hand side of a rule
#[derive(Debug, Clone)]
pub enum RhsElement {
    Symbol(Symbol),
    MidAction(String),
}

/// Code block with source line information for #line directives
#[derive(Debug, Clone)]
pub struct CodeBlock {
    /// The code content
    pub code: String,
    /// Starting line number in the source grammar file
    pub line: usize,
}

/// Parsed yacc grammar
#[derive(Debug, Clone)]
pub struct ParsedGrammar {
    /// C code from %{ ... %} blocks with line information
    pub prologue: Vec<CodeBlock>,
    /// Union definition with line information
    pub union_def: Option<CodeBlock>,
    /// Token declarations
    pub tokens: Vec<TokenDecl>,
    /// Type declarations
    pub types: Vec<TypeDecl>,
    /// Start symbol
    pub start: Option<String>,
    /// Grammar rules
    pub rules: Vec<Rule>,
    /// Programs section (after second %%) with line information
    pub epilogue: Option<CodeBlock>,
}

impl ParsedGrammar {
    fn new() -> Self {
        ParsedGrammar {
            prologue: Vec::new(),
            union_def: None,
            tokens: Vec::new(),
            types: Vec::new(),
            start: None,
            rules: Vec::new(),
            epilogue: None,
        }
    }
}

/// Parser for yacc grammar
struct Parser<'a> {
    tokens: &'a [PositionedToken],
    pos: usize,
    precedence_level: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [PositionedToken]) -> Self {
        Parser {
            tokens,
            pos: 0,
            precedence_level: 0,
        }
    }

    fn current(&self) -> Option<&PositionedToken> {
        self.tokens.get(self.pos)
    }

    fn current_token(&self) -> Option<&Token> {
        self.current().map(|t| &t.token)
    }

    fn current_line(&self) -> usize {
        self.current().map(|t| t.line).unwrap_or(0)
    }

    fn advance(&mut self) -> Option<&PositionedToken> {
        let token = self.tokens.get(self.pos);
        if token.is_some() {
            self.pos += 1;
        }
        token
    }

    fn expect(&mut self, expected: &Token) -> Result<(), YaccError> {
        match self.current() {
            Some(t) if &t.token == expected => {
                self.advance();
                Ok(())
            }
            Some(t) => Err(YaccError::Syntax {
                line: t.line,
                msg: format!("expected {:?}, found {:?}", expected, t.token),
            }),
            None => Err(YaccError::Syntax {
                line: 0,
                msg: format!("expected {:?}, found end of input", expected),
            }),
        }
    }

    fn parse(&mut self) -> Result<ParsedGrammar, YaccError> {
        let mut grammar = ParsedGrammar::new();

        // Parse declarations section
        self.parse_declarations(&mut grammar)?;

        // Expect first %%
        self.expect(&Token::Mark)?;

        // Parse rules section
        self.parse_rules(&mut grammar)?;

        // Check for second %% and programs section
        if let Some(Token::ProgramsSection(code)) = self.current_token() {
            let line = self.current_line();
            grammar.epilogue = Some(CodeBlock {
                code: code.clone(),
                line,
            });
            self.advance();
        }

        Ok(grammar)
    }

    fn parse_declarations(&mut self, grammar: &mut ParsedGrammar) -> Result<(), YaccError> {
        loop {
            match self.current_token() {
                Some(Token::Mark) => break,
                Some(Token::CodeBlock(code)) => {
                    let line = self.current_line();
                    grammar.prologue.push(CodeBlock {
                        code: code.clone(),
                        line,
                    });
                    self.advance();
                }
                Some(Token::UnionBody(body)) => {
                    if grammar.union_def.is_some() {
                        return Err(YaccError::Syntax {
                            line: self.current_line(),
                            msg: "multiple %union declarations".into(),
                        });
                    }
                    let line = self.current_line();
                    grammar.union_def = Some(CodeBlock {
                        code: body.clone(),
                        line,
                    });
                    self.advance();
                }
                Some(Token::SymbolDecl) => {
                    self.advance();
                    self.parse_token_decl(grammar, None)?;
                }
                Some(Token::Left) => {
                    self.advance();
                    self.precedence_level += 1;
                    let level = self.precedence_level;
                    self.parse_token_decl(grammar, Some((level, Associativity::Left)))?;
                }
                Some(Token::Right) => {
                    self.advance();
                    self.precedence_level += 1;
                    let level = self.precedence_level;
                    self.parse_token_decl(grammar, Some((level, Associativity::Right)))?;
                }
                Some(Token::Nonassoc) => {
                    self.advance();
                    self.precedence_level += 1;
                    let level = self.precedence_level;
                    self.parse_token_decl(grammar, Some((level, Associativity::NonAssoc)))?;
                }
                Some(Token::Type) => {
                    self.advance();
                    self.parse_type_decl(grammar)?;
                }
                Some(Token::Start) => {
                    self.advance();
                    if let Some(Token::Identifier(name)) = self.current_token() {
                        grammar.start = Some(name.clone());
                        self.advance();
                    } else {
                        return Err(YaccError::Syntax {
                            line: self.current_line(),
                            msg: "expected identifier after %start".into(),
                        });
                    }
                }
                None => {
                    return Err(YaccError::Syntax {
                        line: 0,
                        msg: "unexpected end of input in declarations".into(),
                    })
                }
                _ => {
                    return Err(YaccError::Syntax {
                        line: self.current_line(),
                        msg: format!(
                            "unexpected token in declarations: {:?}",
                            self.current_token()
                        ),
                    })
                }
            }
        }
        Ok(())
    }

    fn parse_token_decl(
        &mut self,
        grammar: &mut ParsedGrammar,
        prec: Option<(usize, Associativity)>,
    ) -> Result<(), YaccError> {
        // Optional tag
        let tag = if let Some(Token::Tag(t)) = self.current_token() {
            let t = t.clone();
            self.advance();
            Some(t)
        } else {
            None
        };

        // Parse list of token names with optional numbers
        loop {
            match self.current_token() {
                Some(Token::Identifier(name)) => {
                    let name = name.clone();
                    self.advance();

                    // Optional number
                    let number = if let Some(Token::Number(n)) = self.current_token() {
                        let n = *n;
                        self.advance();
                        Some(n)
                    } else {
                        None
                    };

                    grammar.tokens.push(TokenDecl {
                        name,
                        tag: tag.clone(),
                        number,
                        precedence: prec.map(|(l, _)| l),
                        associativity: prec.map(|(_, a)| a),
                    });
                }
                Some(Token::CharLiteral(c)) => {
                    let c = *c;
                    self.advance();

                    // Character literals as tokens
                    grammar.tokens.push(TokenDecl {
                        name: format!("'{}'", c),
                        tag: tag.clone(),
                        number: Some(c as i32),
                        precedence: prec.map(|(l, _)| l),
                        associativity: prec.map(|(_, a)| a),
                    });
                }
                _ => break,
            }
        }

        Ok(())
    }

    fn parse_type_decl(&mut self, grammar: &mut ParsedGrammar) -> Result<(), YaccError> {
        // %type requires a tag
        let tag = if let Some(Token::Tag(t)) = self.current_token() {
            let t = t.clone();
            self.advance();
            t
        } else {
            return Err(YaccError::Syntax {
                line: self.current_line(),
                msg: "%type requires a <tag>".into(),
            });
        };

        let mut symbols = Vec::new();

        // Parse list of non-terminal names
        while let Some(Token::Identifier(name)) = self.current_token() {
            symbols.push(name.clone());
            self.advance();
        }

        if symbols.is_empty() {
            return Err(YaccError::Syntax {
                line: self.current_line(),
                msg: "%type requires at least one symbol".into(),
            });
        }

        grammar.types.push(TypeDecl { tag, symbols });

        Ok(())
    }

    fn parse_rules(&mut self, grammar: &mut ParsedGrammar) -> Result<(), YaccError> {
        // First rule starts with C_IDENTIFIER
        while let Some(Token::CIdentifier(lhs)) = self.current_token() {
            let lhs = lhs.clone();
            let line = self.current_line();
            self.advance();

            self.parse_rule_body(grammar, &lhs, line)?;

            // Handle continuation with | or new rules
            loop {
                match self.current_token() {
                    Some(Token::Pipe) => {
                        let line = self.current_line();
                        self.advance();
                        self.parse_rule_body(grammar, &lhs, line)?;
                    }
                    Some(Token::Semicolon) => {
                        self.advance();
                        break;
                    }
                    Some(Token::CIdentifier(_)) | Some(Token::ProgramsSection(_)) | None => break,
                    _ => {
                        // Continue if we see another rule element
                        if self.is_rule_start() {
                            break;
                        }
                        return Err(YaccError::Syntax {
                            line: self.current_line(),
                            msg: format!("unexpected token in rule: {:?}", self.current_token()),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn is_rule_start(&self) -> bool {
        matches!(self.current_token(), Some(Token::CIdentifier(_)))
    }

    fn parse_rule_body(
        &mut self,
        grammar: &mut ParsedGrammar,
        lhs: &str,
        line: usize,
    ) -> Result<(), YaccError> {
        let mut rhs = Vec::new();
        let mut prec = None;
        let mut action = None;

        loop {
            match self.current_token() {
                Some(Token::Identifier(name)) => {
                    rhs.push(RhsElement::Symbol(Symbol::NonTerminal(name.clone())));
                    self.advance();
                }
                Some(Token::CharLiteral(c)) => {
                    rhs.push(RhsElement::Symbol(Symbol::CharLiteral(*c)));
                    self.advance();
                }
                Some(Token::Error) => {
                    rhs.push(RhsElement::Symbol(Symbol::Error));
                    self.advance();
                }
                Some(Token::Prec) => {
                    self.advance();
                    match self.current_token() {
                        Some(Token::Identifier(name)) => {
                            prec = Some(name.clone());
                            self.advance();
                        }
                        Some(Token::CharLiteral(c)) => {
                            prec = Some(format!("'{}'", c));
                            self.advance();
                        }
                        _ => {
                            return Err(YaccError::Syntax {
                                line: self.current_line(),
                                msg: "expected token name after %prec".into(),
                            });
                        }
                    }
                }
                Some(Token::Action(code)) => {
                    let code = code.clone();
                    self.advance();

                    // Check if this is a mid-rule action or the final action
                    match self.current_token() {
                        Some(Token::Identifier(_))
                        | Some(Token::CharLiteral(_))
                        | Some(Token::Error)
                        | Some(Token::Prec) => {
                            // Mid-rule action
                            rhs.push(RhsElement::MidAction(code));
                        }
                        _ => {
                            // Final action
                            action = Some(code);
                        }
                    }
                }
                Some(Token::Pipe)
                | Some(Token::Semicolon)
                | Some(Token::CIdentifier(_))
                | Some(Token::ProgramsSection(_))
                | None => break,
                _ => {
                    return Err(YaccError::Syntax {
                        line: self.current_line(),
                        msg: format!("unexpected token in rule body: {:?}", self.current_token()),
                    });
                }
            }
        }

        grammar.rules.push(Rule {
            lhs: lhs.to_string(),
            rhs,
            action,
            prec,
            line,
        });

        Ok(())
    }
}

/// Parse tokens into a ParsedGrammar
pub fn parse(tokens: &[PositionedToken]) -> Result<ParsedGrammar, YaccError> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn test_simple_grammar() {
        let input = r#"
%token NUM
%%
expr : NUM
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.tokens.len(), 1);
        assert_eq!(grammar.rules.len(), 1);
        assert_eq!(grammar.rules[0].lhs, "expr");
    }

    #[test]
    fn test_precedence() {
        let input = r#"
%left '+' '-'
%left '*' '/'
%%
expr : expr '+' expr
     | NUM
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.tokens.len(), 4);
        // Check precedence levels
        let plus = grammar.tokens.iter().find(|t| t.name == "'+'").unwrap();
        let times = grammar.tokens.iter().find(|t| t.name == "'*'").unwrap();
        assert!(plus.precedence.unwrap() < times.precedence.unwrap());
    }

    #[test]
    fn test_multiple_rules() {
        let input = r#"
%token NUM ID
%%
expr : term
     | expr '+' term
     ;
term : NUM
     | ID
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.rules.len(), 4);
    }

    #[test]
    fn test_action() {
        let input = r#"
%token NUM
%%
expr : NUM { $$ = $1; }
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert!(grammar.rules[0].action.is_some());
        assert!(grammar.rules[0].action.as_ref().unwrap().contains("$$"));
    }

    #[test]
    fn test_union_and_type() {
        let input = r#"
%union {
    int ival;
    double dval;
}
%token <ival> NUM
%type <ival> expr
%%
expr : NUM
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert!(grammar.union_def.is_some());
        assert_eq!(grammar.types.len(), 1);
        assert_eq!(grammar.types[0].tag, "ival");
    }

    #[test]
    fn test_start_symbol() {
        let input = r#"
%start program
%token NUM
%%
program : expr
        ;
expr : NUM
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.start, Some("program".to_string()));
    }

    #[test]
    fn test_prec_directive() {
        let input = r#"
%token UMINUS
%left '+' '-'
%left '*' '/'
%right UMINUS
%%
expr : expr '+' expr
     | '-' expr %prec UMINUS
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.rules[1].prec, Some("UMINUS".to_string()));
    }

    #[test]
    fn test_programs_section() {
        let input = r#"
%token NUM
%%
expr : NUM
     ;
%%
int main() { return yyparse(); }
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert!(grammar.epilogue.is_some());
        assert!(grammar.epilogue.as_ref().unwrap().code.contains("main"));
    }

    #[test]
    fn test_error_token() {
        let input = r#"
%token NUM
%%
expr : NUM
     | error
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.rules.len(), 2);
    }

    #[test]
    fn test_prologue() {
        let input = r#"
%{
#include <stdio.h>
%}
%token NUM
%%
expr : NUM
     ;
"#;
        let tokens = lex(input).unwrap();
        let grammar = parse(&tokens).unwrap();
        assert_eq!(grammar.prologue.len(), 1);
        assert!(grammar.prologue[0].code.contains("stdio.h"));
    }
}
