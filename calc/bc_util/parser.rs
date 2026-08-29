//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Recursive-descent parser for `bc`.
//!
//! The expression grammar follows the precedence table of POSIX.1-2024,
//! XCU `bc`, Table 3-3, with one function per precedence level. In order of
//! decreasing precedence: `++`/`--`, unary `-`, `^` (right associative),
//! `*` `/` `%`, `+` binary `-`, assignment, and the relational operators —
//! which the grammar admits only inside an `if`, `while` or `for` condition.
//!
//! Assignment is recognised where its left side is syntactically a named
//! expression, so `1 + a = 2` is `1 + (a = 2)`, matching historical bc.

use super::instructions::*;
use super::lexer::{end_position, tokenize, PositionedToken, Token};
use std::rc::Rc;

/// Maximum nesting of expressions and statements.
///
/// Recursive descent uses the machine stack, so without a limit a deeply
/// nested input aborts the process instead of producing a diagnostic. Each
/// level costs several frames, and unoptimized builds use far larger frames
/// than release ones, so the limit is set well below where either would run
/// out. Real bc programs nest a handful of levels.
const MAX_PARSE_DEPTH: usize = 500;

#[derive(Debug)]
struct Diagnostic {
    message: String,
    line: usize,
    col: usize,
}

#[derive(Debug)]
pub struct ParseError {
    errors: Vec<Diagnostic>,
    file: Option<String>,
    /// `true` when more input could complete the program: an unterminated
    /// string, comment, brace or expression. The REPL keeps reading instead of
    /// reporting an error.
    pub is_incomplete: bool,
}

impl ParseError {
    /// The diagnostics, as (line, column, message), for a caller that reports
    /// them itself.
    pub fn diagnostics(&self) -> impl Iterator<Item = (u32, u16, &str)> {
        self.errors
            .iter()
            .map(|e| (e.line as u32, e.col as u16, e.message.as_str()))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for e in &self.errors {
            match &self.file {
                Some(path) => writeln!(f, "{}:{}:{}: {}", path, e.line, e.col, e.message)?,
                None => writeln!(f, "{}:{}: {}", e.line, e.col, e.message)?,
            }
        }
        Ok(())
    }
}

/// The left side of an assignment or an increment: a variable, an array
/// element, or one of the three registers.
enum Target {
    Named(NamedExpr),
    Register(Register),
}

/// The value `x op= v` assigns: `v` for a plain `=`, otherwise `x op v`.
fn combine_assignment(
    op: &Token,
    current: ExprInstruction,
    value: ExprInstruction,
) -> ExprInstruction {
    let build: fn(Box<ExprInstruction>, Box<ExprInstruction>) -> ExprInstruction = match op {
        Token::Assign => return value,
        Token::AddAssign => ExprInstruction::Add,
        Token::SubAssign => ExprInstruction::Sub,
        Token::MulAssign => ExprInstruction::Mul,
        Token::DivAssign => ExprInstruction::Div,
        Token::ModAssign => ExprInstruction::Mod,
        Token::PowAssign => ExprInstruction::Pow,
        _ => unreachable!("assign_op only returns assignment operators"),
    };
    build(Box::new(current), Box::new(value))
}

/// Desugar `x op= v` into an assignment of `x op v` to `x`.
fn build_assignment(target: Target, op: &Token, value: ExprInstruction) -> ExprInstruction {
    match target {
        Target::Named(named) => {
            let current = ExprInstruction::Named(named.clone());
            ExprInstruction::Assignment {
                named,
                value: Box::new(combine_assignment(op, current, value)),
            }
        }
        Target::Register(register) => {
            let current = ExprInstruction::GetRegister(register);
            ExprInstruction::SetRegister {
                register,
                value: Box::new(combine_assignment(op, current, value)),
            }
        }
    }
}

struct Parser<'a> {
    tokens: &'a [PositionedToken],
    pos: usize,
    depth: usize,
    in_function: bool,
    in_loop: bool,
    /// Position just past the end of the input, for errors at end of input.
    end: (usize, usize),
}

type PResult<T> = Result<T, Box<Diagnostic>>;

impl<'a> Parser<'a> {
    fn new(tokens: &'a [PositionedToken], end: (usize, usize)) -> Self {
        Parser {
            tokens,
            pos: 0,
            depth: 0,
            in_function: false,
            in_loop: false,
            end,
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn peek_at(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset).map(|t| &t.token)
    }

    fn at(&self, token: &Token) -> bool {
        self.peek() == Some(token)
    }

    fn advance(&mut self) -> Option<&'a PositionedToken> {
        let t = self.tokens.get(self.pos);
        if t.is_some() {
            self.pos += 1;
        }
        t
    }

    /// The position to blame for an error discovered here.
    fn here(&self) -> (usize, usize) {
        match self.tokens.get(self.pos) {
            Some(t) => (t.line, t.col),
            None => self.end,
        }
    }

    fn line(&self) -> usize {
        self.here().0
    }

    fn error(&self, message: impl Into<String>) -> Box<Diagnostic> {
        let (line, col) = self.here();
        Box::new(Diagnostic {
            message: message.into(),
            line,
            col,
        })
    }

    fn expected(&self, what: &str) -> Box<Diagnostic> {
        match self.peek() {
            Some(t) => self.error(format!("expected {}, found {}", what, t)),
            None => self.error(format!("expected {} at end of input", what)),
        }
    }

    fn expect(&mut self, token: Token, what: &str) -> PResult<()> {
        if self.at(&token) {
            self.advance();
            Ok(())
        } else {
            Err(self.expected(what))
        }
    }

    fn enter(&mut self) -> PResult<()> {
        self.depth += 1;
        if self.depth > MAX_PARSE_DEPTH {
            return Err(self.error(format!("nested more than {} levels deep", MAX_PARSE_DEPTH)));
        }
        Ok(())
    }

    fn leave(&mut self) {
        self.depth -= 1;
    }

    // ---- expressions -------------------------------------------------

    fn parse_expr(&mut self) -> PResult<ExprInstruction> {
        self.enter()?;
        let r = self.parse_additive();
        self.leave();
        r
    }

    fn parse_additive(&mut self) -> PResult<ExprInstruction> {
        let mut lhs = self.parse_multiplicative()?;
        loop {
            let build: fn(Box<ExprInstruction>, Box<ExprInstruction>) -> ExprInstruction =
                match self.peek() {
                    Some(Token::Plus) => ExprInstruction::Add,
                    Some(Token::Minus) => ExprInstruction::Sub,
                    _ => return Ok(lhs),
                };
            self.advance();
            let rhs = self.parse_multiplicative()?;
            lhs = build(Box::new(lhs), Box::new(rhs));
        }
    }

    fn parse_multiplicative(&mut self) -> PResult<ExprInstruction> {
        let mut lhs = self.parse_power()?;
        loop {
            let build: fn(Box<ExprInstruction>, Box<ExprInstruction>) -> ExprInstruction =
                match self.peek() {
                    Some(Token::Star) => ExprInstruction::Mul,
                    Some(Token::Slash) => ExprInstruction::Div,
                    Some(Token::Percent) => ExprInstruction::Mod,
                    _ => return Ok(lhs),
                };
            self.advance();
            let rhs = self.parse_power()?;
            lhs = build(Box::new(lhs), Box::new(rhs));
        }
    }

    /// `^` is right associative, and unary minus binds tighter than it
    /// (Table 3-3), so `-2^2` is `(-2)^2`.
    fn parse_power(&mut self) -> PResult<ExprInstruction> {
        let lhs = self.parse_unary()?;
        if self.at(&Token::Caret) {
            self.advance();
            self.enter()?;
            let rhs = self.parse_power();
            self.leave();
            return Ok(ExprInstruction::Pow(Box::new(lhs), Box::new(rhs?)));
        }
        Ok(lhs)
    }

    fn parse_unary(&mut self) -> PResult<ExprInstruction> {
        if self.at(&Token::Minus) {
            self.advance();
            self.enter()?;
            let operand = self.parse_unary();
            self.leave();
            return Ok(ExprInstruction::UnaryMinus(Box::new(operand?)));
        }
        self.parse_primary()
    }

    /// A named expression: a variable or an array element.
    fn parse_named_expr(&mut self) -> PResult<NamedExpr> {
        let name = match self.peek() {
            Some(Token::Letter(c)) => *c,
            _ => return Err(self.expected("a variable")),
        };
        self.advance();
        if self.at(&Token::LBracket) {
            self.advance();
            let index = self.parse_expr()?;
            self.expect(Token::RBracket, "']'")?;
            return Ok(NamedExpr::ArrayItem {
                name,
                index: Box::new(index),
            });
        }
        Ok(NamedExpr::VariableNumber(name))
    }

    /// The assignment target at the current position, if there is one.
    fn parse_target(&mut self) -> PResult<Target> {
        match self.peek() {
            Some(Token::Letter(_)) => Ok(Target::Named(self.parse_named_expr()?)),
            Some(Token::Scale) => {
                self.advance();
                Ok(Target::Register(Register::Scale))
            }
            Some(Token::Ibase) => {
                self.advance();
                Ok(Target::Register(Register::IBase))
            }
            Some(Token::Obase) => {
                self.advance();
                Ok(Target::Register(Register::OBase))
            }
            _ => Err(self.expected("a named expression")),
        }
    }

    fn assign_op(&self) -> Option<Token> {
        match self.peek() {
            Some(
                t @ (Token::Assign
                | Token::AddAssign
                | Token::SubAssign
                | Token::MulAssign
                | Token::DivAssign
                | Token::ModAssign
                | Token::PowAssign),
            ) => Some(t.clone()),
            _ => None,
        }
    }

    fn parse_builtin(&mut self, function: BuiltinFunction) -> PResult<ExprInstruction> {
        self.advance(); // the builtin's name
        self.expect(Token::LParen, "'(' after a builtin function name")?;
        let arg = self.parse_expr()?;
        self.expect(Token::RParen, "')'")?;
        Ok(ExprInstruction::Builtin {
            function,
            arg: Box::new(arg),
        })
    }

    /// An argument in a call: either a whole array (`a[]`) or an expression.
    fn parse_function_argument(&mut self) -> PResult<FunctionArgument> {
        if let (Some(Token::Letter(c)), Some(Token::LBracket), Some(Token::RBracket)) =
            (self.peek(), self.peek_at(1), self.peek_at(2))
        {
            let name = *c;
            self.advance();
            self.advance();
            self.advance();
            return Ok(FunctionArgument::ArrayVariable(name));
        }
        Ok(FunctionArgument::Expr(self.parse_expr()?))
    }

    fn parse_call(&mut self, name: char) -> PResult<ExprInstruction> {
        self.advance(); // the name
        self.advance(); // '('
        let mut args = Vec::new();
        if !self.at(&Token::RParen) {
            loop {
                args.push(self.parse_function_argument()?);
                if self.at(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
        }
        self.expect(Token::RParen, "')'")?;
        Ok(ExprInstruction::Call { name, args })
    }

    fn parse_primary(&mut self) -> PResult<ExprInstruction> {
        match self.peek() {
            Some(Token::Number(_)) => {
                let Some(PositionedToken {
                    token: Token::Number(n),
                    ..
                }) = self.advance()
                else {
                    unreachable!("just matched a number")
                };
                Ok(ExprInstruction::Number(n.clone()))
            }
            Some(Token::LParen) => {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(Token::RParen, "')'")?;
                Ok(e)
            }
            Some(Token::Length) => self.parse_builtin(BuiltinFunction::Length),
            Some(Token::Sqrt) => self.parse_builtin(BuiltinFunction::Sqrt),
            // `scale` is both a builtin and a register; the '(' tells them apart.
            Some(Token::Scale) if self.peek_at(1) == Some(&Token::LParen) => {
                self.parse_builtin(BuiltinFunction::Scale)
            }
            Some(Token::Incr) => {
                self.advance();
                Ok(match self.parse_target()? {
                    Target::Named(named) => ExprInstruction::PreIncrement(named),
                    Target::Register(register) => ExprInstruction::IncrementRegister {
                        register,
                        prefix: true,
                    },
                })
            }
            Some(Token::Decr) => {
                self.advance();
                Ok(match self.parse_target()? {
                    Target::Named(named) => ExprInstruction::PreDecrement(named),
                    Target::Register(register) => ExprInstruction::DecrementRegister {
                        register,
                        prefix: true,
                    },
                })
            }
            Some(Token::Letter(c)) if self.peek_at(1) == Some(&Token::LParen) => {
                let name = *c;
                self.parse_call(name)
            }
            Some(Token::Letter(_))
            | Some(Token::Scale)
            | Some(Token::Ibase)
            | Some(Token::Obase) => {
                let target = self.parse_target()?;
                if let Some(op) = self.assign_op() {
                    self.advance();
                    let value = self.parse_expr()?;
                    return Ok(build_assignment(target, &op, value));
                }
                match target {
                    Target::Named(named) => {
                        if self.at(&Token::Incr) {
                            self.advance();
                            return Ok(ExprInstruction::PostIncrement(named));
                        }
                        if self.at(&Token::Decr) {
                            self.advance();
                            return Ok(ExprInstruction::PostDecrement(named));
                        }
                        Ok(ExprInstruction::Named(named))
                    }
                    Target::Register(register) => {
                        if self.at(&Token::Incr) {
                            self.advance();
                            return Ok(ExprInstruction::IncrementRegister {
                                register,
                                prefix: false,
                            });
                        }
                        if self.at(&Token::Decr) {
                            self.advance();
                            return Ok(ExprInstruction::DecrementRegister {
                                register,
                                prefix: false,
                            });
                        }
                        Ok(ExprInstruction::GetRegister(register))
                    }
                }
            }
            _ => Err(self.expected("an expression")),
        }
    }

    fn parse_condition(&mut self) -> PResult<ConditionInstruction> {
        let left = self.parse_expr()?;
        let build: fn(ExprInstruction, ExprInstruction) -> ConditionInstruction = match self.peek()
        {
            Some(Token::Eq) => ConditionInstruction::Eq,
            Some(Token::Ne) => ConditionInstruction::Ne,
            Some(Token::Lt) => ConditionInstruction::Lt,
            Some(Token::Le) => ConditionInstruction::Leq,
            Some(Token::Gt) => ConditionInstruction::Gt,
            Some(Token::Ge) => ConditionInstruction::Geq,
            _ => return Ok(ConditionInstruction::Expr(left)),
        };
        self.advance();
        let right = self.parse_expr()?;
        Ok(build(left, right))
    }

    fn parse_parenthesized_condition(&mut self) -> PResult<ConditionInstruction> {
        self.expect(Token::LParen, "'('")?;
        let condition = self.parse_condition()?;
        self.expect(Token::RParen, "')'")?;
        Ok(condition)
    }

    // ---- statements --------------------------------------------------

    /// Parse one statement, appending its instructions to `statements` and one
    /// source line per flattened statement to `source_locations`.
    ///
    /// Returns the number of flattened statements produced. A compound
    /// statement counts itself as well as its body; a braced list counts only
    /// its members, since it produces no instruction of its own.
    fn parse_stmt(
        &mut self,
        statements: &mut Vec<StmtInstruction>,
        source_locations: &mut Vec<usize>,
    ) -> PResult<usize> {
        self.enter()?;
        let r = self.parse_stmt_inner(statements, source_locations);
        self.leave();
        r
    }

    fn parse_stmt_inner(
        &mut self,
        statements: &mut Vec<StmtInstruction>,
        source_locations: &mut Vec<usize>,
    ) -> PResult<usize> {
        let line = self.line();
        match self.peek() {
            Some(Token::Break) => {
                if !self.in_loop {
                    return Err(self.error("break outside of loop"));
                }
                self.advance();
                source_locations.push(line);
                statements.push(StmtInstruction::Break);
                Ok(1)
            }
            Some(Token::Quit) => {
                self.advance();
                source_locations.push(line);
                statements.push(StmtInstruction::Quit);
                Ok(1)
            }
            Some(Token::Return) => {
                if !self.in_function {
                    return Err(self.error("return outside of function"));
                }
                self.advance();
                source_locations.push(line);
                // return | return ( ) | return ( expression )
                if self.at(&Token::LParen) {
                    self.advance();
                    if self.at(&Token::RParen) {
                        self.advance();
                        statements.push(StmtInstruction::Return);
                    } else {
                        let e = self.parse_expr()?;
                        self.expect(Token::RParen, "')'")?;
                        statements.push(StmtInstruction::ReturnExpr(e));
                    }
                } else {
                    statements.push(StmtInstruction::Return);
                }
                Ok(1)
            }
            Some(Token::If) => {
                self.advance();
                let condition = self.parse_parenthesized_condition()?;
                source_locations.push(line);
                let mut body = Vec::new();
                let instruction_count = self.parse_stmt(&mut body, source_locations)?;
                statements.push(StmtInstruction::If {
                    condition,
                    instruction_count,
                    body,
                });
                Ok(instruction_count + 1)
            }
            Some(Token::While) => {
                self.advance();
                let condition = self.parse_parenthesized_condition()?;
                source_locations.push(line);
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                let mut body = Vec::new();
                let result = self.parse_stmt(&mut body, source_locations);
                self.in_loop = was_in_loop;
                let instruction_count = result?;
                statements.push(StmtInstruction::While {
                    condition,
                    instruction_count,
                    body,
                });
                Ok(instruction_count + 1)
            }
            Some(Token::For) => {
                self.advance();
                self.expect(Token::LParen, "'('")?;
                let init = self.parse_expr()?;
                self.expect(Token::Semicolon, "';'")?;
                let condition = self.parse_condition()?;
                self.expect(Token::Semicolon, "';'")?;
                let update = self.parse_expr()?;
                self.expect(Token::RParen, "')'")?;
                source_locations.push(line);
                let was_in_loop = self.in_loop;
                self.in_loop = true;
                let mut body = Vec::new();
                let result = self.parse_stmt(&mut body, source_locations);
                self.in_loop = was_in_loop;
                let instruction_count = result?;
                statements.push(StmtInstruction::For {
                    init,
                    condition,
                    update,
                    instruction_count,
                    body,
                });
                Ok(instruction_count + 1)
            }
            Some(Token::LBrace) => {
                self.advance();
                let count = self.parse_statement_list(statements, source_locations)?;
                self.expect(Token::RBrace, "'}'")?;
                Ok(count)
            }
            Some(Token::Str(_)) => {
                let Some(PositionedToken {
                    token: Token::Str(s),
                    ..
                }) = self.advance()
                else {
                    unreachable!("just matched a string")
                };
                let s = s.clone();
                source_locations.push(line);
                statements.push(StmtInstruction::String(s));
                Ok(1)
            }
            _ => {
                let e = self.parse_expr()?;
                source_locations.push(line);
                statements.push(StmtInstruction::Expr(e));
                Ok(1)
            }
        }
    }

    fn at_statement_separator(&self) -> bool {
        self.at(&Token::Semicolon) || self.at(&Token::Newline)
    }

    /// The body of a braced list or a function: statements separated by `;` or
    /// newline. POSIX requires a separator between two statements, so `{1 2}`
    /// is a syntax error.
    fn parse_statement_list(
        &mut self,
        statements: &mut Vec<StmtInstruction>,
        source_locations: &mut Vec<usize>,
    ) -> PResult<usize> {
        let mut count = 0;
        loop {
            while self.at_statement_separator() {
                self.advance();
            }
            if self.at(&Token::RBrace) || self.at_end() {
                return Ok(count);
            }
            count += self.parse_stmt(statements, source_locations)?;
            if !self.at_statement_separator() {
                return Ok(count);
            }
        }
    }

    /// One input line's worth of statements, separated by `;`.
    fn parse_semicolon_list(
        &mut self,
        statements: &mut Vec<StmtInstruction>,
        source_locations: &mut Vec<usize>,
    ) -> PResult<()> {
        loop {
            while self.at(&Token::Semicolon) {
                self.advance();
            }
            if self.at(&Token::Newline) || self.at_end() {
                return Ok(());
            }
            self.parse_stmt(statements, source_locations)?;
            if !self.at(&Token::Semicolon) {
                return Ok(());
            }
        }
    }

    // ---- functions ---------------------------------------------------

    /// `a` or `a[]` in a parameter or auto list.
    fn parse_variable(&mut self) -> PResult<Variable> {
        let name = match self.peek() {
            Some(Token::Letter(c)) => *c,
            _ => return Err(self.expected("a variable name")),
        };
        self.advance();
        if self.at(&Token::LBracket) {
            self.advance();
            self.expect(Token::RBracket, "']' after '[' in an array parameter")?;
            return Ok(Variable::Array(name));
        }
        Ok(Variable::Number(name))
    }

    fn parse_variable_list(&mut self) -> PResult<Vec<Variable>> {
        let mut list = vec![self.parse_variable()?];
        while self.at(&Token::Comma) {
            self.advance();
            list.push(self.parse_variable()?);
        }
        Ok(list)
    }

    fn parse_function(&mut self, file: Rc<str>) -> PResult<Function> {
        self.advance(); // 'define'
        let name = match self.peek() {
            Some(Token::Letter(c)) => *c,
            _ => return Err(self.expected("a function name")),
        };
        self.advance();
        self.expect(Token::LParen, "'(' after a function name")?;
        let parameters = if self.at(&Token::RParen) {
            Vec::new()
        } else {
            self.parse_variable_list()?
        };
        self.expect(Token::RParen, "')'")?;
        self.expect(Token::LBrace, "'{'")?;
        // POSIX: `'{' NEWLINE opt_auto_define_list statement_list '}'`.
        self.expect(Token::Newline, "a newline after '{'")?;

        let mut locals = Vec::new();
        // POSIX: `'{' NEWLINE opt_auto_define_list statement_list '}'`. The
        // auto list must follow the newline immediately; anywhere else `auto`
        // is a keyword in statement position, and so a syntax error rather
        // than a run of single-letter variable references.
        if self.at(&Token::Auto) {
            self.advance();
            locals = self.parse_variable_list()?;
            if !self.at_statement_separator() {
                return Err(self.expected("a newline or ';' after an auto list"));
            }
            self.advance();
        }

        let was_in_function = self.in_function;
        let was_in_loop = self.in_loop;
        self.in_function = true;
        self.in_loop = false;
        let mut body = Vec::new();
        let mut source_locations = Vec::new();
        let result = self.parse_statement_list(&mut body, &mut source_locations);
        self.in_function = was_in_function;
        self.in_loop = was_in_loop;
        result?;
        self.expect(Token::RBrace, "'}'")?;

        Ok(Function {
            name,
            file,
            parameters: parameters.into(),
            locals: locals.into(),
            body: body.into(),
            source_locations: source_locations.into(),
        })
    }

    fn parse_program(&mut self, file: Rc<str>) -> PResult<Program> {
        let mut instructions = Vec::new();
        let mut source_locations = Vec::new();
        while !self.at_end() {
            if self.at(&Token::Define) {
                let function = self.parse_function(file.clone())?;
                instructions.push(StmtInstruction::DefineFunction {
                    name: function.name,
                    function,
                });
                continue;
            }
            self.parse_semicolon_list(&mut instructions, &mut source_locations)?;
            // POSIX: `input_item : semicolon_list NEWLINE`.
            if self.at_end() {
                return Err(self.error("missing newline at end of input"));
            }
            self.expect(Token::Newline, "a newline")?;
        }
        Ok(Program {
            file,
            instructions,
            source_locations,
        })
    }
}

/// Parse a complete `bc` program.
pub fn parse_program(text: &str, file_path: Option<&str>) -> Result<Program, ParseError> {
    let tokens = match tokenize(text) {
        Ok(tokens) => tokens,
        Err(e) => {
            return Err(ParseError {
                errors: vec![Diagnostic {
                    message: e.message,
                    line: e.line,
                    col: e.col,
                }],
                file: file_path.map(String::from),
                is_incomplete: e.incomplete,
            })
        }
    };
    let file = Rc::<str>::from(file_path.unwrap_or(""));
    let mut parser = Parser::new(&tokens, end_position(text));
    match parser.parse_program(file) {
        Ok(program) => Ok(program),
        Err(e) => {
            // Running out of tokens means the program may yet be completed by
            // more input; an error at a token that is present is final.
            let is_incomplete = parser.at_end();
            Err(ParseError {
                errors: vec![*e],
                file: file_path.map(String::from),
                is_incomplete,
            })
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_expr(input: &str) -> ExprInstruction {
        let program = parse_program(input, None).expect("error parsing expression");
        assert_eq!(program.instructions.len(), 1);
        if let StmtInstruction::Expr(expr) = program.instructions.into_iter().next().unwrap() {
            expr
        } else {
            panic!("expected expression")
        }
    }

    fn parse_stmt(input: &str) -> StmtInstruction {
        let program = parse_program(input, None).expect("error parsing statement");
        assert_eq!(program.instructions.len(), 1);
        program.instructions.into_iter().next().unwrap()
    }

    fn parse_function(input: &str) -> Function {
        let program = parse_program(input, None).expect("error parsing function");
        assert_eq!(program.instructions.len(), 1);
        if let StmtInstruction::DefineFunction { function, .. } =
            program.instructions.into_iter().next().unwrap()
        {
            function
        } else {
            panic!("expected function")
        }
    }

    fn program_err(input: &str) -> ParseError {
        parse_program(input, None).unwrap_err()
    }

    #[test]
    fn test_parse_empty_program() {
        let program = parse_program("", None).expect("error parsing empty program");
        assert_eq!(program.instructions.len(), 0);
    }

    #[test]
    fn test_parse_number() {
        let expr = parse_expr("123\n");
        assert_eq!(expr, ExprInstruction::Number("123".to_string()));
        let expr = parse_expr("123.456\n");
        assert_eq!(expr, ExprInstruction::Number("123.456".to_string()));
        let expr = parse_expr(".456\n");
        assert_eq!(expr, ExprInstruction::Number(".456".to_string()));
        let expr = parse_expr("123.\n");
        assert_eq!(expr, ExprInstruction::Number("123.".to_string()));
        let expr = parse_expr("1\\\n23\n");
        assert_eq!(expr, ExprInstruction::Number("123".to_string()));
        let expr = parse_expr("1\\\n.23\n");
        assert_eq!(expr, ExprInstruction::Number("1.23".to_string()));
        let expr = parse_expr("1.\\\n23\n");
        assert_eq!(expr, ExprInstruction::Number("1.23".to_string()));
    }

    #[test]
    fn test_parse_named() {
        let expr = parse_expr("a\n");
        assert_eq!(expr, ExprInstruction::Named(NamedExpr::VariableNumber('a')));
        let expr = parse_expr("a[1]\n");
        assert_eq!(
            expr,
            (ExprInstruction::Named(NamedExpr::ArrayItem {
                name: 'a',
                index: Box::new(ExprInstruction::Number("1".to_string()))
            }))
        );
    }

    #[test]
    fn test_parse_register_get() {
        let expr = parse_expr("scale\n");
        assert_eq!(expr, ExprInstruction::GetRegister(Register::Scale));
        let expr = parse_expr("ibase\n");
        assert_eq!(expr, ExprInstruction::GetRegister(Register::IBase));
        let expr = parse_expr("obase\n");
        assert_eq!(expr, ExprInstruction::GetRegister(Register::OBase));
    }

    #[test]
    fn test_parse_builtin_call() {
        let expr = parse_expr("length(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Builtin {
                function: BuiltinFunction::Length,
                arg: Box::new(ExprInstruction::Number("1".to_string()))
            }
        );
        let expr = parse_expr("sqrt(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Builtin {
                function: BuiltinFunction::Sqrt,
                arg: Box::new(ExprInstruction::Number("1".to_string()))
            }
        );
        let expr = parse_expr("scale(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Builtin {
                function: BuiltinFunction::Scale,
                arg: Box::new(ExprInstruction::Number("1".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_function_call() {
        let expr = parse_expr("f()\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'f',
                args: vec![]
            }
        );
        let expr = parse_expr("f(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'f',
                args: vec![FunctionArgument::Expr(ExprInstruction::Number(
                    "1".to_string()
                ))]
            }
        );
        let expr = parse_expr("f(1, 2)\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'f',
                args: vec![
                    FunctionArgument::Expr(ExprInstruction::Number("1".to_string())),
                    FunctionArgument::Expr(ExprInstruction::Number("2".to_string()))
                ]
            }
        );
        let expr = parse_expr("f(a[])\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'f',
                args: vec![FunctionArgument::ArrayVariable('a')]
            }
        );
    }

    #[test]
    fn test_parse_prefix_increment() {
        let expr = parse_expr("++a\n");
        assert_eq!(
            expr,
            ExprInstruction::PreIncrement(NamedExpr::VariableNumber('a'))
        );
    }

    #[test]
    fn test_parse_prefix_decrement() {
        let expr = parse_expr("--a\n");
        assert_eq!(
            expr,
            ExprInstruction::PreDecrement(NamedExpr::VariableNumber('a'))
        );
    }

    #[test]
    fn test_parse_postfix_increment() {
        let expr = parse_expr("a++\n");
        assert_eq!(
            expr,
            ExprInstruction::PostIncrement(NamedExpr::VariableNumber('a'))
        );
    }

    #[test]
    fn test_parse_postfix_decrement() {
        let expr = parse_expr("a--\n");
        assert_eq!(
            expr,
            ExprInstruction::PostDecrement(NamedExpr::VariableNumber('a'))
        );
    }

    #[test]
    fn test_parse_unary_minus() {
        let expr = parse_expr("-1\n");
        assert_eq!(
            expr,
            ExprInstruction::UnaryMinus(Box::new(ExprInstruction::Number("1".to_string())))
        );
    }

    #[test]
    fn test_parse_binary_operators() {
        let expr = parse_expr("1 + 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
        let expr = parse_expr("1 - 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Sub(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
        let expr = parse_expr("1 * 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Mul(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
        let expr = parse_expr("1 / 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Div(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
        let expr = parse_expr("1 % 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Mod(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
        let expr = parse_expr("1 ^ 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Pow(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
    }

    #[test]
    fn test_parse_correct_precedence() {
        let expr = parse_expr("1 + 2 * 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Number("2".to_string())),
                    Box::new(ExprInstruction::Number("3".to_string()))
                ))
            )
        );
        // Unary minus binds tighter than '^' (POSIX Table 3-3), so this is
        // (-1)^2, not -(1^2).
        let expr = parse_expr("-1 ^ 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Pow(
                Box::new(ExprInstruction::UnaryMinus(Box::new(
                    ExprInstruction::Number("1".to_string())
                ))),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
    }

    #[test]
    fn test_pow_is_right_associative() {
        let expr = parse_expr("2 ^ 3 ^ 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Pow(
                Box::new(ExprInstruction::Number("2".to_string())),
                Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Number("3".to_string())),
                    Box::new(ExprInstruction::Number("2".to_string()))
                ))
            )
        );
    }

    #[test]
    fn test_parse_assignment() {
        let expr = parse_expr("a = 1\n");
        assert_eq!(
            expr,
            ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Number("1".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_compound_assignment() {
        let expr = parse_expr("a += 1\n");
        assert_eq!(
            expr,
            ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Add(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            }
        );
    }

    #[test]
    fn test_parse_register_assignment() {
        let expr = parse_expr("scale = 1\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::Scale,
                value: Box::new(ExprInstruction::Number("1".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_compound_register_assignment() {
        let expr = parse_expr("scale += 1\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::Scale,
                value: Box::new(ExprInstruction::Add(
                    Box::new(ExprInstruction::GetRegister(Register::Scale)),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            }
        );
    }

    #[test]
    fn test_assignment_is_right_associative() {
        let expr = parse_expr("a = b = 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('b'),
                    value: Box::new(ExprInstruction::Number("3".to_string()))
                })
            }
        );
    }

    #[test]
    fn test_parse_string_statement() {
        let stmt = parse_stmt("\"hello\"\n");
        assert_eq!(stmt, StmtInstruction::String("hello".to_string()));
    }

    #[test]
    fn test_parse_break_and_quit() {
        let stmt = parse_stmt("quit\n");
        assert_eq!(stmt, StmtInstruction::Quit);
        let stmt = parse_stmt("while (1) break\n");
        assert_eq!(
            stmt,
            StmtInstruction::While {
                condition: ConditionInstruction::Expr(ExprInstruction::Number("1".to_string())),
                instruction_count: 1,
                body: vec![StmtInstruction::Break]
            }
        );
    }

    #[test]
    fn test_parse_if() {
        let stmt = parse_stmt("if (1 < 2) 3\n");
        assert_eq!(
            stmt,
            StmtInstruction::If {
                condition: ConditionInstruction::Lt(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string())
                ),
                instruction_count: 1,
                body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                    "3".to_string()
                ))]
            }
        );
    }

    #[test]
    fn test_parse_all_relational_operators() {
        for (text, expected) in [
            (
                "if (1 == 2) 3\n",
                ConditionInstruction::Eq(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
            (
                "if (1 != 2) 3\n",
                ConditionInstruction::Ne(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
            (
                "if (1 < 2) 3\n",
                ConditionInstruction::Lt(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
            (
                "if (1 <= 2) 3\n",
                ConditionInstruction::Leq(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
            (
                "if (1 > 2) 3\n",
                ConditionInstruction::Gt(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
            (
                "if (1 >= 2) 3\n",
                ConditionInstruction::Geq(
                    ExprInstruction::Number("1".to_string()),
                    ExprInstruction::Number("2".to_string()),
                ),
            ),
        ] {
            let stmt = parse_stmt(text);
            match stmt {
                StmtInstruction::If { condition, .. } => assert_eq!(condition, expected),
                _ => panic!("expected an if statement"),
            }
        }
    }

    #[test]
    fn test_parse_while() {
        let stmt = parse_stmt("while (1) 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::While {
                condition: ConditionInstruction::Expr(ExprInstruction::Number("1".to_string())),
                instruction_count: 1,
                body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                    "2".to_string()
                ))]
            }
        );
    }

    #[test]
    fn test_parse_for() {
        let stmt = parse_stmt("for (i = 0; i < 5; i++) 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::For {
                init: ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('i'),
                    value: Box::new(ExprInstruction::Number("0".to_string()))
                },
                condition: ConditionInstruction::Lt(
                    ExprInstruction::Named(NamedExpr::VariableNumber('i')),
                    ExprInstruction::Number("5".to_string())
                ),
                update: ExprInstruction::PostIncrement(NamedExpr::VariableNumber('i')),
                instruction_count: 1,
                body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                    "2".to_string()
                ))]
            }
        );
    }

    #[test]
    fn test_parse_braced_statement_list() {
        let program = parse_program("{1; 2}\n", None).expect("error parsing program");
        assert_eq!(
            program.instructions,
            vec![
                StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                StmtInstruction::Expr(ExprInstruction::Number("2".to_string()))
            ]
        );
    }

    #[test]
    fn test_parse_empty_function() {
        let f = parse_function("define f() {\n}\n");
        assert_eq!(f.name, 'f');
        assert!(f.parameters.is_empty());
        assert!(f.locals.is_empty());
        assert!(f.body.is_empty());
    }

    #[test]
    fn test_parse_function_with_parameters() {
        let f = parse_function("define f(a, b[]) {\n}\n");
        assert_eq!(
            f.parameters.as_ref(),
            [Variable::Number('a'), Variable::Array('b')]
        );
    }

    #[test]
    fn test_parse_function_with_locals() {
        let f = parse_function("define f() {\nauto a, b[]\n}\n");
        assert_eq!(
            f.locals.as_ref(),
            [Variable::Number('a'), Variable::Array('b')]
        );
        let f = parse_function("define f() {\nauto a;\n}\n");
        assert_eq!(f.locals.as_ref(), [Variable::Number('a')]);
    }

    #[test]
    fn test_parse_function_body() {
        let f = parse_function("define f(a) {\nreturn(a)\n}\n");
        assert_eq!(
            f.body.as_ref(),
            [StmtInstruction::ReturnExpr(ExprInstruction::Named(
                NamedExpr::VariableNumber('a')
            ))]
        );
        let f = parse_function("define f() {\nreturn\n}\n");
        assert_eq!(f.body.as_ref(), [StmtInstruction::Return]);
        let f = parse_function("define f() {\nreturn()\n}\n");
        assert_eq!(f.body.as_ref(), [StmtInstruction::Return]);
    }

    #[test]
    fn test_ignore_comments() {
        let expr = parse_expr("1 /* comment */ + 2\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
    }

    #[test]
    fn test_ignore_backslash_newline() {
        let expr = parse_expr("1 + \\\n2\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
    }

    #[test]
    fn test_break_outside_of_loop_is_an_error() {
        assert!(parse_program("break\n", None).is_err());
        assert!(parse_program("if (1) break\n", None).is_err());
        assert!(parse_program("define f() {\nbreak\n}\n", None).is_err());
    }

    #[test]
    fn test_return_outside_of_function_is_an_error() {
        assert!(parse_program("return\n", None).is_err());
        assert!(parse_program("return(1)\n", None).is_err());
    }

    #[test]
    fn test_partial_comment_is_incomplete() {
        assert!(program_err("/* comment").is_incomplete);
        assert!(program_err("1 + /* comment").is_incomplete);
        assert!(program_err("1 + 2;/* comment").is_incomplete);
    }

    #[test]
    fn test_partial_string_is_incomplete() {
        assert!(program_err("\"string").is_incomplete);
        assert!(program_err("1 + 2;\"string").is_incomplete);
    }

    #[test]
    fn test_partial_function_requires_is_incomplete() {
        assert!(program_err("define").is_incomplete);
        assert!(program_err("define f").is_incomplete);
        assert!(program_err("define f()").is_incomplete);
        assert!(program_err("define f() {\n").is_incomplete);
    }

    #[test]
    fn test_unclosed_braced_statement_list_is_incomplete() {
        assert!(program_err("{").is_incomplete);
        assert!(program_err("{1;").is_incomplete);
    }

    #[test]
    fn test_statements_ending_with_a_backslash_newline_are_incomplete() {
        assert!(program_err("1 + 2 *\\\n").is_incomplete);
        assert!(program_err("1 +\\\n").is_incomplete);
    }

    #[test]
    fn test_incomplete_conditions_are_incomplete() {
        assert!(program_err("if (c) {\n").is_incomplete);
        assert!(program_err("while (c) {\n").is_incomplete);
    }

    /// A complete but invalid line must be reported, not treated as needing
    /// more input; otherwise the REPL swallows every later line.
    #[test]
    fn test_complete_but_invalid_input_is_not_incomplete() {
        assert!(!program_err("1 \"abc\"\n").is_incomplete);
        assert!(!program_err("x=1 \"abc\"\n").is_incomplete);
        assert!(!program_err("\"a\" \"b\"\n").is_incomplete);
        assert!(!program_err("1 2\n").is_incomplete);
        assert!(!program_err("1 +\n").is_incomplete);
    }

    /// POSIX requires a separator between statements. Without it, `auto` and
    /// the keywords silently decompose into single-letter variables.
    #[test]
    fn test_juxtaposed_statements_are_an_error() {
        assert!(parse_program("{1 2 3}\n", None).is_err());
        assert!(parse_program("{foo}\n", None).is_err());
        assert!(parse_program("{length}\n", None).is_err());
        assert!(parse_program("{auto}\n", None).is_err());
        assert!(parse_program("{define}\n", None).is_err());
    }

    /// `auto` is a keyword everywhere, so a misplaced auto list is a syntax
    /// error rather than a sequence of variable references.
    #[test]
    fn test_misplaced_auto_is_an_error() {
        assert!(parse_program("define f() {\n\nauto x\nx = 7\nreturn(x)\n}\n", None).is_err());
        assert!(parse_program("define f() {\n1\nauto x\n}\n", None).is_err());
    }

    /// POSIX has only `return` and `return ( expression )`.
    #[test]
    fn test_return_without_parentheses_is_an_error() {
        assert!(parse_program("define f() {\nreturn 5\n}\n", None).is_err());
    }

    /// POSIX lexical rule 15 makes `--` a single token.
    #[test]
    fn test_double_minus_is_not_two_negations() {
        assert!(parse_program("1--1\n", None).is_err());
        assert!(parse_program("1---1\n", None).is_err());
        // A blank separates them, and then it is subtraction of a negation.
        let expr = parse_expr("1 - -1\n");
        assert_eq!(
            expr,
            ExprInstruction::Sub(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::UnaryMinus(Box::new(
                    ExprInstruction::Number("1".to_string())
                )))
            )
        );
    }

    /// Relational operators appear only in conditions.
    #[test]
    fn test_relational_operators_outside_conditions_are_an_error() {
        assert!(parse_program("1 < 2\n", None).is_err());
        assert!(parse_program("a = 1 < 2\n", None).is_err());
        assert!(parse_program("if (1 < 2 < 3) 1\n", None).is_err());
    }

    /// A one-line definition is rejected: POSIX requires a newline after '{'.
    #[test]
    fn test_function_requires_newline_after_brace() {
        assert!(parse_program("define f(a) { return(a) }\n", None).is_err());
    }

    #[test]
    fn test_missing_newline_at_end_of_input_is_an_error() {
        assert!(parse_program("1 + 1", None).is_err());
    }

    /// Deep nesting must produce a diagnostic instead of exhausting the stack.
    #[test]
    fn test_deep_nesting_is_bounded() {
        let deep = format!("{}1{}\n", "(".repeat(50_000), ")".repeat(50_000));
        let err = program_err(&deep);
        assert!(
            err.to_string().contains("nested more than"),
            "expected a depth diagnostic, got: {}",
            err
        );
    }

    #[test]
    fn test_generate_correct_source_info() {
        let program = parse_program("1 + 2\n", Some("file.bc")).expect("error parsing program");
        assert_eq!(program.file.as_ref(), "file.bc");
        assert_eq!(program.source_locations, [1]);
        let program = parse_program("1 + 2\n3 + 4\n", None).expect("error parsing program");
        assert_eq!(program.file.as_ref(), "");
        assert_eq!(program.source_locations, [1, 2]);
        let program =
            parse_program("1; 2; 3; 4 + 5 + 9\n", Some("file.bc")).expect("error parsing program");
        assert_eq!(program.source_locations, [1, 1, 1, 1]);
        let program = parse_program("{1; 2; 3}\n", None).expect("error parsing program");
        assert_eq!(program.source_locations, [1, 1, 1]);
        let program =
            parse_program("if(0) {\n 1\n 2\n 3\n}\n 1 + 2\n", None).expect("error parsing program");
        assert_eq!(program.source_locations, [1, 2, 3, 4, 6]);
        let program =
            parse_program("while(0) {1\n 2\n 3\n}\n", None).expect("error parsing program");
        assert_eq!(program.source_locations, [1, 1, 2, 3]);
        let program = parse_program("for(1; 2; 3) {\n1\n 2\n\n\n 3\n}\n", None)
            .expect("error parsing program");
        assert_eq!(program.source_locations, [1, 2, 3, 6]);
    }

    /// A compound statement counts itself as well as its body, so a nested
    /// compound does not desynchronise the source-location index.
    #[test]
    fn test_nested_compound_statement_counts_itself() {
        let program =
            parse_program("x=1\nif (1) if (1) x\n1/0\n", None).expect("error parsing program");
        // x=1; outer if; inner if; the inner body; 1/0.
        assert_eq!(program.source_locations, [1, 2, 2, 2, 3]);
        let StmtInstruction::If {
            instruction_count, ..
        } = &program.instructions[1]
        else {
            panic!("expected an if statement")
        };
        // The inner `if` contributes itself plus its one-statement body.
        assert_eq!(*instruction_count, 2);
    }
}
