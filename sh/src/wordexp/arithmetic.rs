use crate::parse::word::Word;
use crate::shell::environment::Environment;
use crate::shell::{CommandExecutionError, Shell};
use crate::wordexp::expanded_word::ExpandedWord;
use crate::wordexp::{expand_word_to_string, ExpansionResult};
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::CharIndices;

enum UnaryOperator {
    Plus,
    Minus,
    Not,
    BitwiseNot,
}

enum BinaryOperator {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShiftLeft,
    ShiftRight,
    Le,
    Leq,
    Ge,
    Geq,
    Eq,
    Neq,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
}

enum Expr<'src> {
    Variable(&'src str),
    Number(i64),
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expr<'src>>,
    },
    BinaryOp {
        operator: BinaryOperator,
        lhs: Box<Expr<'src>>,
        rhs: Box<Expr<'src>>,
    },
    Conditional {
        condition: Box<Expr<'src>>,
        true_expr: Box<Expr<'src>>,
        false_expr: Box<Expr<'src>>,
    },
    Assignment {
        variable: &'src str,
        value: Box<Expr<'src>>,
    },
    CompoundAssignment {
        variable: &'src str,
        operator: BinaryOperator,
        value: Box<Expr<'src>>,
    },
}

impl<'src> Expr<'src> {
    fn assignment(variable: &'src str, assignment_op: ExprToken, value: Self) -> Self {
        if assignment_op == ExprToken::Assign {
            Expr::Assignment {
                variable,
                value: Box::new(value),
            }
        } else {
            let operator = match assignment_op {
                ExprToken::MulAssign => BinaryOperator::Mul,
                ExprToken::DivAssign => BinaryOperator::Div,
                ExprToken::ModAssign => BinaryOperator::Mod,
                ExprToken::AddAssign => BinaryOperator::Add,
                ExprToken::SubAssign => BinaryOperator::Sub,
                ExprToken::ShiftLeftAssign => BinaryOperator::ShiftLeft,
                ExprToken::ShiftRightAssign => BinaryOperator::ShiftRight,
                ExprToken::AndAssign => BinaryOperator::BitwiseAnd,
                ExprToken::XorAssign => BinaryOperator::BitwiseXor,
                ExprToken::OrAssign => BinaryOperator::BitwiseOr,
                _ => unreachable!("invalid assignment op {:?}", assignment_op),
            };
            Expr::CompoundAssignment {
                variable,
                operator,
                value: Box::new(value),
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ExprToken<'src> {
    Variable(&'src str),
    Number(i64),
    Plus,
    Minus,
    BitwiseNot,
    Not,
    Mul,
    Div,
    Mod,
    ShiftLeft,
    ShiftRight,
    Le,
    Leq,
    Ge,
    Geq,
    Eq,
    Neq,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    QuestionMark,
    Colon,
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    AndAssign,
    XorAssign,
    OrAssign,
    LParen,
    RParen,

    EOF,
}

impl Display for ExprToken<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprToken::Variable(var) => write!(f, "{var}"),
            ExprToken::Number(num) => write!(f, "{num}"),
            ExprToken::Plus => write!(f, "+"),
            ExprToken::Minus => write!(f, "-"),
            ExprToken::BitwiseNot => write!(f, "~"),
            ExprToken::Not => write!(f, "!"),
            ExprToken::Mul => write!(f, "*"),
            ExprToken::Div => write!(f, "/"),
            ExprToken::Mod => write!(f, "%"),
            ExprToken::ShiftLeft => write!(f, "<<"),
            ExprToken::ShiftRight => write!(f, ">>"),
            ExprToken::Le => write!(f, "<"),
            ExprToken::Leq => write!(f, "<="),
            ExprToken::Ge => write!(f, ">"),
            ExprToken::Geq => write!(f, ">="),
            ExprToken::Eq => write!(f, "=="),
            ExprToken::Neq => write!(f, "!="),
            ExprToken::BitwiseAnd => write!(f, "&"),
            ExprToken::BitwiseXor => write!(f, "^"),
            ExprToken::BitwiseOr => write!(f, "|"),
            ExprToken::LogicalAnd => write!(f, "&&"),
            ExprToken::LogicalOr => write!(f, "||"),
            ExprToken::QuestionMark => write!(f, "?"),
            ExprToken::Colon => write!(f, ":"),
            ExprToken::Assign => write!(f, "="),
            ExprToken::MulAssign => write!(f, "*="),
            ExprToken::DivAssign => write!(f, "/="),
            ExprToken::ModAssign => write!(f, "%="),
            ExprToken::AddAssign => write!(f, "+="),
            ExprToken::SubAssign => write!(f, "-="),
            ExprToken::ShiftLeftAssign => write!(f, "<<="),
            ExprToken::ShiftRightAssign => write!(f, ">>="),
            ExprToken::AndAssign => write!(f, "&="),
            ExprToken::XorAssign => write!(f, "^="),
            ExprToken::OrAssign => write!(f, "|="),
            ExprToken::LParen => write!(f, "("),
            ExprToken::RParen => write!(f, ")"),
            ExprToken::EOF => write!(f, "<EOF>"),
        }
    }
}

impl From<ExprToken<'_>> for BinaryOperator {
    fn from(value: ExprToken<'_>) -> Self {
        match value {
            ExprToken::Plus => BinaryOperator::Add,
            ExprToken::Minus => BinaryOperator::Sub,
            ExprToken::Mul => BinaryOperator::Mul,
            ExprToken::Div => BinaryOperator::Div,
            ExprToken::Mod => BinaryOperator::Mod,
            ExprToken::ShiftLeft => BinaryOperator::ShiftLeft,
            ExprToken::ShiftRight => BinaryOperator::ShiftRight,
            ExprToken::Le => BinaryOperator::Le,
            ExprToken::Leq => BinaryOperator::Leq,
            ExprToken::Ge => BinaryOperator::Ge,
            ExprToken::Geq => BinaryOperator::Geq,
            ExprToken::Eq => BinaryOperator::Eq,
            ExprToken::Neq => BinaryOperator::Neq,
            ExprToken::BitwiseAnd => BinaryOperator::BitwiseAnd,
            ExprToken::BitwiseXor => BinaryOperator::BitwiseXor,
            ExprToken::BitwiseOr => BinaryOperator::BitwiseOr,
            ExprToken::LogicalAnd => BinaryOperator::LogicalAnd,
            ExprToken::LogicalOr => BinaryOperator::LogicalOr,
            _ => unreachable!("not a binary operator: {:?}", value),
        }
    }
}

impl From<ExprToken<'_>> for UnaryOperator {
    fn from(value: ExprToken<'_>) -> Self {
        match value {
            ExprToken::Plus => UnaryOperator::Plus,
            ExprToken::Minus => UnaryOperator::Minus,
            ExprToken::Not => UnaryOperator::Not,
            ExprToken::BitwiseNot => UnaryOperator::BitwiseNot,
            _ => unreachable!("not a unary operator: {:?}", value),
        }
    }
}

const ASSIGNMENT_TOKENS: &[ExprToken<'static>] = &[
    ExprToken::Assign,
    ExprToken::MulAssign,
    ExprToken::DivAssign,
    ExprToken::ModAssign,
    ExprToken::AddAssign,
    ExprToken::SubAssign,
    ExprToken::ShiftLeftAssign,
    ExprToken::ShiftRightAssign,
    ExprToken::AndAssign,
    ExprToken::XorAssign,
    ExprToken::OrAssign,
];

const BINARY_OPERATORS: &[&[ExprToken<'static>]] = &[
    &[ExprToken::LogicalOr],
    &[ExprToken::LogicalAnd],
    &[ExprToken::BitwiseOr],
    &[ExprToken::BitwiseXor],
    &[ExprToken::BitwiseAnd],
    &[ExprToken::Eq, ExprToken::Neq],
    &[ExprToken::Le, ExprToken::Leq, ExprToken::Ge, ExprToken::Geq],
    &[ExprToken::ShiftLeft, ExprToken::ShiftRight],
    &[ExprToken::Plus, ExprToken::Minus],
    &[ExprToken::Mul, ExprToken::Div, ExprToken::Mod],
];

const UNARY_OPERATORS: &[ExprToken<'static>] = &[
    ExprToken::Plus,
    ExprToken::Minus,
    ExprToken::Not,
    ExprToken::BitwiseNot,
];

type ExprParseResult<T> = Result<T, String>;

struct ExpressionParser<'src> {
    source: &'src str,
    source_iter: Peekable<CharIndices<'src>>,
    source_position: usize,
    lookahead: ExprToken<'src>,
}

impl<'src> ExpressionParser<'src> {
    fn skip_whitespace(&mut self) {
        while let Some((p, c)) = self.source_iter.peek() {
            self.source_position = *p;
            if !c.is_whitespace() {
                break;
            }
            self.source_iter.next();
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.source_iter.peek().map(|(_, c)| *c)
    }

    fn multichar_token(
        &mut self,
        start: ExprToken<'static>,
        choices: &[(char, ExprToken<'static>)],
    ) -> ExprToken<'static> {
        let mut token = start;
        for (c, t) in choices {
            if self.peek() == Some(*c) {
                token = *t;
                self.advance_char();
                break;
            }
        }
        token
    }

    fn advance_char(&mut self) -> Option<char> {
        self.source_iter.next().map(|(p, c)| {
            self.source_position = p;
            c
        })
    }

    fn lex_decimal_number(&mut self, start_pos: usize) -> ExprParseResult<i64> {
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.advance_char();
        }
        i64::from_str_radix(&self.source[start_pos..=self.source_position], 10)
            .map_err(|_| "invalid number".to_string())
    }

    fn lex_octal_number(&mut self, start_pos: usize) -> ExprParseResult<i64> {
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.advance_char();
        }
        i64::from_str_radix(&self.source[start_pos..=self.source_position], 8)
            .map_err(|_| "invalid octal number".to_string())
    }

    fn lex_hex_number(&mut self, start_pos: usize) -> ExprParseResult<i64> {
        while matches!(self.peek(), Some(c) if c.is_ascii_hexdigit()) {
            self.advance_char();
        }
        i64::from_str_radix(&self.source[start_pos..=self.source_position], 16)
            .map_err(|_| "invalid hexadecimal number".to_string())
    }

    fn lex_variable(&mut self, start_pos: usize) -> &'src str {
        while matches!(self.peek(), Some(c) if c.is_ascii_alphanumeric() || c == '_') {
            self.advance_char();
        }
        &self.source[start_pos..=self.source_position]
    }

    fn next(&mut self) -> ExprParseResult<ExprToken<'src>> {
        self.skip_whitespace();
        let start_pos = self.source_position;
        match self.advance_char() {
            Some('+') => Ok(self.multichar_token(ExprToken::Plus, &[('=', ExprToken::AddAssign)])),
            Some('-') => Ok(self.multichar_token(ExprToken::Minus, &[('=', ExprToken::SubAssign)])),
            Some('~') => Ok(ExprToken::BitwiseNot),
            Some('!') => Ok(self.multichar_token(ExprToken::Not, &[('=', ExprToken::Neq)])),
            Some('*') => Ok(self.multichar_token(ExprToken::Mul, &[('=', ExprToken::MulAssign)])),
            Some('/') => Ok(self.multichar_token(ExprToken::Div, &[('=', ExprToken::DivAssign)])),
            Some('%') => Ok(self.multichar_token(ExprToken::Mod, &[('=', ExprToken::ModAssign)])),
            Some('<') => match self.peek() {
                Some('<') => {
                    self.advance_char();
                    Ok(self.multichar_token(
                        ExprToken::ShiftLeft,
                        &[('=', ExprToken::ShiftLeftAssign)],
                    ))
                }
                Some('=') => {
                    self.advance_char();
                    Ok(ExprToken::Leq)
                }
                _ => Ok(ExprToken::Le),
            },
            Some('>') => match self.peek() {
                Some('>') => {
                    self.advance_char();
                    Ok(self.multichar_token(
                        ExprToken::ShiftRight,
                        &[('=', ExprToken::ShiftRightAssign)],
                    ))
                }
                Some('=') => {
                    self.advance_char();
                    Ok(ExprToken::Geq)
                }
                _ => Ok(ExprToken::Ge),
            },
            Some('=') => Ok(self.multichar_token(ExprToken::Assign, &[('=', ExprToken::Eq)])),
            Some('&') => Ok(self.multichar_token(
                ExprToken::BitwiseAnd,
                &[('=', ExprToken::AndAssign), ('&', ExprToken::LogicalAnd)],
            )),
            Some('^') => {
                Ok(self.multichar_token(ExprToken::BitwiseXor, &[('=', ExprToken::XorAssign)]))
            }
            Some('|') => Ok(self.multichar_token(
                ExprToken::BitwiseOr,
                &[('=', ExprToken::OrAssign), ('|', ExprToken::LogicalOr)],
            )),
            Some('?') => Ok(ExprToken::QuestionMark),
            Some(':') => Ok(ExprToken::Colon),
            Some('(') => Ok(ExprToken::LParen),
            Some(')') => Ok(ExprToken::RParen),
            Some(c) if c.is_ascii_digit() => {
                if c == '0' {
                    if self.peek() == Some('x') {
                        self.advance_char();
                        self.lex_hex_number(self.source_position + 1)
                            .map(ExprToken::Number)
                    } else {
                        self.lex_octal_number(start_pos).map(ExprToken::Number)
                    }
                } else {
                    self.lex_decimal_number(start_pos).map(ExprToken::Number)
                }
            }
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                Ok(ExprToken::Variable(self.lex_variable(start_pos)))
            }
            None => Ok(ExprToken::EOF),
            Some(other) => Err(format!("unexpected character '{other}")),
        }
    }

    fn advance_token(&mut self) -> ExprParseResult<ExprToken<'src>> {
        let token = self.lookahead;
        self.lookahead = self.next()?;
        Ok(token)
    }

    fn match_token(&mut self, token: ExprToken) -> ExprParseResult<()> {
        if self.lookahead == token {
            self.advance_token()?;
            Ok(())
        } else {
            Err(format!("expected {}, got {}", token, self.lookahead))
        }
    }

    fn matches(&mut self, token: ExprToken) -> bool {
        self.lookahead == token
    }

    fn matches_alternatives(
        &mut self,
        tokens: &[ExprToken<'static>],
    ) -> Option<ExprToken<'static>> {
        tokens.iter().find(|t| self.matches(**t)).copied()
    }

    fn parse_literal(&mut self) -> ExprParseResult<Expr<'src>> {
        match self.advance_token()? {
            ExprToken::Variable(var) => Ok(Expr::Variable(var)),
            ExprToken::Number(num) => Ok(Expr::Number(num)),
            ExprToken::LParen => {
                let expr = self.parse_expr()?;
                self.match_token(ExprToken::RParen)?;
                Ok(expr)
            }
            other => Err(format!("unexpected token {other}")),
        }
    }

    fn parse_unary(&mut self) -> ExprParseResult<Expr<'src>> {
        if let Some(op) = self.matches_alternatives(UNARY_OPERATORS) {
            self.advance_token()?;
            let operand = self.parse_literal()?;
            Ok(Expr::UnaryOp {
                operator: op.into(),
                operand: operand.into(),
            })
        } else {
            self.parse_literal()
        }
    }

    fn parse_binary_expr(&mut self, level: usize) -> ExprParseResult<Expr<'src>> {
        if level == BINARY_OPERATORS.len() {
            return self.parse_unary();
        }
        let mut expr = self.parse_binary_expr(level + 1)?;
        while let Some(op) = self.matches_alternatives(BINARY_OPERATORS[level]) {
            self.advance_token()?;
            let rhs = self.parse_binary_expr(level + 1)?;
            expr = Expr::BinaryOp {
                operator: op.into(),
                lhs: expr.into(),
                rhs: rhs.into(),
            };
        }
        Ok(expr)
    }

    fn parse_conditional(&mut self) -> ExprParseResult<Expr<'src>> {
        let condition = self.parse_binary_expr(0)?;
        if self.matches(ExprToken::QuestionMark) {
            self.advance_token()?;
            let true_expr = self.parse_expr()?;
            self.match_token(ExprToken::Colon)?;
            let false_expr = self.parse_expr()?;
            Ok(Expr::Conditional {
                condition: condition.into(),
                true_expr: true_expr.into(),
                false_expr: false_expr.into(),
            })
        } else {
            Ok(condition)
        }
    }

    fn parse_expr(&mut self) -> ExprParseResult<Expr<'src>> {
        let mut expr = self.parse_conditional()?;
        while let Some(assignment_op) = self.matches_alternatives(ASSIGNMENT_TOKENS) {
            self.advance_token()?;
            if let Expr::Variable(var) = expr {
                let rhs = self.parse_expr()?;
                expr = Expr::assignment(var, assignment_op, rhs);
            } else {
                return Err("expected variable on the left side of assignment".to_string());
            }
        }
        Ok(expr)
    }
}

fn parse_expression(expr: &str) -> Result<Expr, String> {
    let mut parser = ExpressionParser {
        source: expr,
        source_iter: expr.char_indices().peekable(),
        source_position: 0,
        lookahead: ExprToken::EOF,
    };
    parser.advance_token()?;
    parser.parse_expr()
}

fn binary_operation(operator: &BinaryOperator, lhs_value: i64, rhs_value: i64) -> i64 {
    match operator {
        BinaryOperator::Mul => lhs_value * rhs_value,
        BinaryOperator::Div => lhs_value / rhs_value,
        BinaryOperator::Mod => lhs_value % rhs_value,
        BinaryOperator::Add => lhs_value + rhs_value,
        BinaryOperator::Sub => lhs_value - rhs_value,
        BinaryOperator::ShiftLeft => lhs_value << rhs_value,
        BinaryOperator::ShiftRight => lhs_value >> rhs_value,
        BinaryOperator::Le => (lhs_value < rhs_value) as i64,
        BinaryOperator::Leq => (lhs_value <= rhs_value) as i64,
        BinaryOperator::Ge => (lhs_value > rhs_value) as i64,
        BinaryOperator::Geq => (lhs_value >= rhs_value) as i64,
        BinaryOperator::Eq => (lhs_value == rhs_value) as i64,
        BinaryOperator::Neq => (lhs_value != rhs_value) as i64,
        BinaryOperator::BitwiseAnd => lhs_value & rhs_value,
        BinaryOperator::BitwiseXor => lhs_value ^ rhs_value,
        BinaryOperator::BitwiseOr => lhs_value | rhs_value,
        BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => unreachable!(),
    }
}

fn interpret_expression(expr: &Expr, shell: &mut Shell) -> ExpansionResult<i64> {
    match expr {
        Expr::Variable(var) => {
            let value = shell.environment.get_str_value(var).unwrap_or_default();
            Ok(value.parse().unwrap_or(0))
        }
        Expr::Number(num) => Ok(*num),
        Expr::UnaryOp { operator, operand } => {
            let value = interpret_expression(operand, shell)?;
            match operator {
                UnaryOperator::Plus => Ok(value),
                UnaryOperator::Minus => Ok(-value),
                UnaryOperator::Not => Ok(!(value != 0) as i64),
                UnaryOperator::BitwiseNot => Ok(!value),
            }
        }
        Expr::BinaryOp { lhs, operator, rhs } => {
            let lhs_value = interpret_expression(lhs, shell)?;
            match operator {
                BinaryOperator::LogicalAnd => {
                    return if lhs_value != 0 {
                        Ok((interpret_expression(rhs, shell)? != 0) as i64)
                    } else {
                        Ok(0)
                    }
                }
                BinaryOperator::LogicalOr => {
                    return if lhs_value == 0 {
                        Ok((interpret_expression(rhs, shell)? != 0) as i64)
                    } else {
                        Ok(1)
                    }
                }
                _ => {}
            }
            let rhs_value = interpret_expression(rhs, shell)?;
            Ok(binary_operation(operator, lhs_value, rhs_value))
        }
        Expr::Conditional {
            condition,
            true_expr,
            false_expr,
        } => {
            if interpret_expression(condition, shell)? != 0 {
                interpret_expression(true_expr, shell)
            } else {
                interpret_expression(false_expr, shell)
            }
        }
        Expr::Assignment { variable, value } => {
            let value = interpret_expression(value, shell)?;
            shell.assign_global(variable.to_string(), value.to_string())?;
            Ok(value)
        }
        Expr::CompoundAssignment {
            variable,
            operator,
            value,
        } => {
            let value = interpret_expression(value, shell)?;
            let current_value = shell
                .environment
                .get_str_value(variable)
                .map(|val| val.parse().unwrap_or(0))
                .unwrap_or(0);
            let new_value = binary_operation(operator, current_value, value);
            shell.assign_global(variable.to_string(), new_value.to_string())?;
            Ok(new_value)
        }
    }
}

pub fn expand_arithmetic_expression_into(
    expanded_word: &mut ExpandedWord,
    expr: &Word,
    inside_double_quotes: bool,
    shell: &mut Shell,
) -> ExpansionResult<()> {
    let expr = expand_word_to_string(expr, false, shell)?;
    let expr = parse_expression(&expr).map_err(CommandExecutionError::ExpansionError)?;
    let value = interpret_expression(&expr, shell)?;
    expanded_word.append(value.to_string(), inside_double_quotes, true);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::word::test_utils::quoted_literal;

    fn execute_expr(s: &str) -> String {
        let mut shell = Shell::default();
        let mut result = ExpandedWord::default();
        expand_arithmetic_expression_into(&mut result, &quoted_literal(s), false, &mut shell)
            .expect("invalid expression");
        result.to_string()
    }

    fn test_assignment_with_initial_value(expr: &str, var: &str, initial_value: &str) -> String {
        let mut shell = Shell::default();
        shell
            .environment
            .set_global(var.to_string(), initial_value.to_string())
            .expect("variable is readonly");
        let mut result = ExpandedWord::default();
        expand_arithmetic_expression_into(&mut result, &quoted_literal(expr), false, &mut shell)
            .expect("invalid expression");
        let result = result.to_string();
        assert_eq!(shell.environment.get_str_value(var), Some(result.as_str()));
        result
    }

    fn test_assignment(expr: &str) -> (String, Environment) {
        let mut shell = Shell::default();
        let mut result = ExpandedWord::default();
        expand_arithmetic_expression_into(&mut result, &quoted_literal(expr), false, &mut shell)
            .expect("invalid expression");
        (result.to_string(), shell.environment)
    }

    #[test]
    fn literal_number() {
        assert_eq!(execute_expr("1"), "1");
        assert_eq!(execute_expr("0123"), "83");
        assert_eq!(execute_expr("0x123"), "291");
    }

    #[test]
    fn unary_operations() {
        assert_eq!(execute_expr("+1"), "1");
        assert_eq!(execute_expr("-1"), "-1");
        assert_eq!(execute_expr("!0"), "1");
        assert_eq!(execute_expr("!3"), "0");
        assert_eq!(execute_expr("~0"), "-1");
    }

    #[test]
    fn binary_operations() {
        assert_eq!(execute_expr("2 * 2"), "4");
        assert_eq!(execute_expr("4 / 2"), "2");
        assert_eq!(execute_expr("5 % 2"), "1");
        assert_eq!(execute_expr("1 + 1"), "2");
        assert_eq!(execute_expr("3 - 2"), "1");
        assert_eq!(execute_expr("2 << 3"), "16");
        assert_eq!(execute_expr("16 >> 3"), "2");
        assert_eq!(execute_expr("1 < 2"), "1");
        assert_eq!(execute_expr("2 <= 2"), "1");
        assert_eq!(execute_expr("1 > 2"), "0");
        assert_eq!(execute_expr("2 >= 2"), "1");
        assert_eq!(execute_expr("0 == 0"), "1");
        assert_eq!(execute_expr("1 != 2"), "1");
        assert_eq!(execute_expr("345 & 789"), "273");
        assert_eq!(execute_expr("123 ^ 456"), "435");
        assert_eq!(execute_expr("1 | 2"), "3");
        assert_eq!(execute_expr("1 && 0"), "0");
        assert_eq!(execute_expr("1 || 0"), "1");
    }

    #[test]
    fn conditional_expression() {
        assert_eq!(execute_expr("1 ? 2 : 3"), "2");
        assert_eq!(execute_expr("30 ? 2 : 3"), "2");
        assert_eq!(execute_expr("0 ? 2 : 3"), "3");
        assert_eq!(execute_expr("0 ? 2 : 1 ? 3 : 4"), "3");
    }

    #[test]
    fn simple_assignment() {
        let (result, env) = test_assignment("x = 1");
        assert_eq!(result, "1");
        assert_eq!(env.get_str_value("x"), Some("1"));
    }

    #[test]
    fn compound_assignment() {
        assert_eq!(test_assignment_with_initial_value("x *= 2", "x", "4"), "8");
        assert_eq!(test_assignment_with_initial_value("x /= 2", "x", "4"), "2");
        assert_eq!(test_assignment_with_initial_value("x %= 2", "x", "3"), "1");
        assert_eq!(test_assignment_with_initial_value("x += 2", "x", "4"), "6");
        assert_eq!(test_assignment_with_initial_value("x -= 2", "x", "2"), "0");
        assert_eq!(
            test_assignment_with_initial_value("x <<= 4", "x", "2"),
            "32"
        );
        assert_eq!(
            test_assignment_with_initial_value("x >>= 4", "x", "32"),
            "2"
        );
        assert_eq!(test_assignment_with_initial_value("x &= 3", "x", "2"), "2");
        assert_eq!(test_assignment_with_initial_value("x ^= 1", "x", "2"), "3");
        assert_eq!(
            test_assignment_with_initial_value("x |= 1", "x", "10"),
            "11"
        );
    }

    #[test]
    fn precedence() {
        assert_eq!(
            execute_expr(
                "(2 + 3 * 4 - 5 / 2 % 3 << 1 >> 1 & 4 | 5 ^ 1) && (2 < 3 || 4 >= 5) ? ~2 + !3 : -4"
            ),
            "-3"
        );
    }
}
