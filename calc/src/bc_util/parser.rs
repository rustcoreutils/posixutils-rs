//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use pest::{error::InputLocation, iterators::Pair, pratt_parser::PrattParser, Parser};

use super::instructions::*;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc, Op};

        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left)
            | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left)
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::modulus, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::prefix(Rule::neg))
    };
}

fn first_char(s: &str) -> char {
    s.chars().next().unwrap()
}

fn as_letter(r: Pair<Rule>) -> char {
    first_char(r.as_str())
}

fn first_child(r: Pair<Rule>) -> Pair<Rule> {
    r.into_inner().next().unwrap()
}

fn as_variable(r: Pair<Rule>) -> Variable {
    match r.as_rule() {
        Rule::variable_number => Variable::Number(as_letter(r)),
        Rule::array => Variable::Array(as_letter(r)),
        _ => unreachable!(),
    }
}

fn to_bc_str(s: &str) -> String {
    s.trim_matches('\"').to_string()
}

fn to_bc_number(s: &str) -> String {
    s.trim().replace("\\\n", "")
}

fn as_register(r: Pair<Rule>) -> Register {
    match r.as_rule() {
        Rule::scale => Register::Scale,
        Rule::ibase => Register::IBase,
        Rule::obase => Register::OBase,
        _ => unreachable!(),
    }
}

fn generate_assignment<F>(
    op_rule: Rule,
    named_expr: ExprInstruction,
    value: ExprInstruction,
    assign: F,
) -> ExprInstruction
where
    F: FnOnce(Box<ExprInstruction>) -> ExprInstruction,
{
    match op_rule {
        Rule::assign => assign(Box::new(value)),
        Rule::add_assign => assign(Box::new(ExprInstruction::Add(
            Box::new(named_expr),
            Box::new(value),
        ))),
        Rule::sub_assign => assign(Box::new(ExprInstruction::Sub(
            Box::new(named_expr),
            Box::new(value),
        ))),
        Rule::mul_assign => assign(Box::new(ExprInstruction::Mul(
            Box::new(named_expr),
            Box::new(value),
        ))),
        Rule::div_assign => assign(Box::new(ExprInstruction::Div(
            Box::new(named_expr),
            Box::new(value),
        ))),
        Rule::mod_assign => assign(Box::new(ExprInstruction::Mod(
            Box::new(named_expr),
            Box::new(value),
        ))),
        Rule::pow_assign => assign(Box::new(ExprInstruction::Pow(
            Box::new(named_expr),
            Box::new(value),
        ))),
        _ => unreachable!(),
    }
}

fn parse_named_expr(expr: Pair<Rule>) -> NamedExpr {
    let expr = first_child(expr);
    match expr.as_rule() {
        Rule::variable_number => NamedExpr::VariableNumber(first_char(expr.as_str())),
        Rule::array_item => {
            // name [ index ]
            let mut inner = expr.into_inner();
            let name = as_letter(inner.next().unwrap());
            let index = Box::new(parse_expr(inner.next().unwrap()));
            NamedExpr::ArrayItem { name, index }
        }
        _ => unreachable!(),
    }
}

fn parse_function_argument(arg: Pair<Rule>) -> FunctionArgument {
    match arg.as_rule() {
        Rule::array => FunctionArgument::ArrayVariable(as_letter(arg)),
        Rule::expression => FunctionArgument::Expr(parse_expr(arg)),
        _ => unreachable!(),
    }
}

fn parse_primary(expr: Pair<Rule>) -> ExprInstruction {
    let expr = expr.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::number => ExprInstruction::Number(to_bc_number(expr.as_str())),
        Rule::paren => parse_expr(first_child(expr)),
        Rule::builtin_call => {
            // name ( expr )
            let mut inner = expr.into_inner();
            let func = match inner.next().unwrap().as_str() {
                "length" => BuiltinFunction::Length,
                "sqrt" => BuiltinFunction::Sqrt,
                "scale" => BuiltinFunction::Scale,
                _ => unreachable!(),
            };
            let arg = parse_expr(inner.next().unwrap());
            ExprInstruction::Builtin {
                function: func,
                arg: Box::new(arg),
            }
        }
        Rule::fn_call => {
            // name ( expr* )
            let mut inner = expr.into_inner();
            let name = as_letter(inner.next().unwrap());
            let args = if let Some(args) = inner.next() {
                args.into_inner().map(parse_function_argument).collect()
            } else {
                vec![]
            };
            ExprInstruction::Call { name, args }
        }
        Rule::prefix_increment => {
            ExprInstruction::PreIncrement(parse_named_expr(first_child(expr)))
        }
        Rule::prefix_decrement => {
            ExprInstruction::PreDecrement(parse_named_expr(first_child(expr)))
        }
        Rule::postfix_increment => {
            ExprInstruction::PostIncrement(parse_named_expr(first_child(expr)))
        }
        Rule::postfix_decrement => {
            ExprInstruction::PostDecrement(parse_named_expr(first_child(expr)))
        }
        Rule::negation => ExprInstruction::UnaryMinus(Box::new(parse_primary(first_child(expr)))),
        Rule::register => {
            let register = match first_child(expr).as_rule() {
                Rule::scale => Register::Scale,
                Rule::ibase => Register::IBase,
                Rule::obase => Register::OBase,
                _ => unreachable!(),
            };
            ExprInstruction::GetRegister(register)
        }
        Rule::register_assignment => {
            // register assign_op expression
            let mut inner = expr.into_inner();
            let register = as_register(first_child(inner.next().unwrap()));
            let op = inner.next().unwrap();
            let value = parse_expr(inner.next().unwrap());
            generate_assignment(
                op.as_rule(),
                ExprInstruction::GetRegister(register),
                value,
                |value| ExprInstruction::SetRegister { register, value },
            )
        }
        Rule::assignment => {
            // name assignment_operator expr
            let mut inner = expr.into_inner();
            let named = parse_named_expr(inner.next().unwrap());
            let op = inner.next().unwrap();
            let value = parse_expr(inner.next().unwrap());
            generate_assignment(
                op.as_rule(),
                ExprInstruction::Named(named.clone()),
                value,
                |value| ExprInstruction::Assignment { named, value },
            )
        }

        Rule::named_expression => ExprInstruction::Named(parse_named_expr(expr)),
        r => unreachable!("found rule {:?}", r),
    }
}

fn parse_expr(expr: Pair<Rule>) -> ExprInstruction {
    PRATT_PARSER
        .map_primary(parse_primary)
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => ExprInstruction::UnaryMinus(Box::new(rhs)),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => ExprInstruction::Add(Box::new(lhs), Box::new(rhs)),
            Rule::sub => ExprInstruction::Sub(Box::new(lhs), Box::new(rhs)),
            Rule::mul => ExprInstruction::Mul(Box::new(lhs), Box::new(rhs)),
            Rule::div => ExprInstruction::Div(Box::new(lhs), Box::new(rhs)),
            Rule::modulus => ExprInstruction::Mod(Box::new(lhs), Box::new(rhs)),
            Rule::pow => ExprInstruction::Pow(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(expr.into_inner())
}

fn parse_condition(expr: Pair<Rule>) -> ConditionInstruction {
    let expr = first_child(expr);
    match expr.as_rule() {
        Rule::expression => ConditionInstruction::Expr(parse_expr(expr)),
        Rule::relational_expression => {
            let mut inner = expr.into_inner();
            let left = parse_expr(inner.next().unwrap());
            let op = inner.next().unwrap();
            let right = parse_expr(inner.next().unwrap());
            match op.as_rule() {
                Rule::same => ConditionInstruction::Eq(left, right),
                Rule::neq => ConditionInstruction::Ne(left, right),
                Rule::lt => ConditionInstruction::Lt(left, right),
                Rule::leq => ConditionInstruction::Leq(left, right),
                Rule::gt => ConditionInstruction::Gt(left, right),
                Rule::geq => ConditionInstruction::Geq(left, right),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn parse_stmt(
    stmt: Pair<Rule>,
    in_function: bool,
    in_loop: bool,
    statements: &mut Vec<StmtInstruction>,
) -> Result<(), PestError> {
    let stmt = first_child(stmt);
    match stmt.as_rule() {
        Rule::break_stmt => {
            if !in_loop {
                return Err(pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError {
                        message: "break outside of loop".to_string(),
                    },
                    stmt.as_span(),
                ));
            }
            statements.push(StmtInstruction::Break);
        }
        Rule::quit => {
            statements.push(StmtInstruction::Quit);
        }
        Rule::return_stmt => {
            // return ( "(" expr? ")" )?
            if !in_function {
                return Err(pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError {
                        message: "return outside of function".to_string(),
                    },
                    stmt.as_span(),
                ));
            }
            let mut inner = stmt.into_inner();
            if let Some(expr) = inner.next() {
                statements.push(StmtInstruction::ReturnExpr(parse_expr(expr)));
            } else {
                statements.push(StmtInstruction::Return);
            }
        }
        Rule::if_stmt => {
            // if (condition) stmt
            let mut inner = stmt.into_inner();
            let condition = parse_condition(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), in_function, in_loop, &mut body)?;
            statements.push(StmtInstruction::If { condition, body });
        }
        Rule::while_stmt => {
            // while (condition) stmt
            let mut inner = stmt.into_inner();
            let condition = parse_condition(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), in_function, true, &mut body)?;
            statements.push(StmtInstruction::While { condition, body });
        }
        Rule::for_stmt => {
            // for (init; condition; update) stmt
            let mut inner = stmt.into_inner();
            let init = parse_expr(inner.next().unwrap());
            let condition = parse_condition(inner.next().unwrap());
            let update = parse_expr(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), in_function, true, &mut body)?;
            statements.push(StmtInstruction::For {
                init,
                condition,
                update,
                body,
            });
        }

        Rule::braced_statement_list => {
            for stmt in first_child(stmt).into_inner() {
                parse_stmt(stmt, in_function, in_loop, statements)?;
            }
        }
        Rule::string => {
            statements.push(StmtInstruction::String(to_bc_str(stmt.as_str())));
        }
        Rule::expression => {
            statements.push(StmtInstruction::Expr(parse_expr(stmt)));
        }
        _ => unreachable!(),
    }
    Ok(())
}

fn parse_function(func: Pair<Rule>) -> Result<Function, PestError> {
    let mut function = func.into_inner();

    // define letter ( parameter_list ) auto_define_list statement_list end

    let name = as_letter(function.next().unwrap());

    let mut parameters = Vec::new();
    let parameter_list = function.next().unwrap();
    let auto_define_list = if parameter_list.as_rule() == Rule::parameter_list {
        parameters = parameter_list.into_inner().map(as_variable).collect();
        function.next().unwrap()
    } else {
        parameter_list
    };

    let mut locals = Vec::new();
    let statement_list = if auto_define_list.as_rule() == Rule::auto_define_list {
        locals = first_child(auto_define_list)
            .into_inner()
            .map(as_variable)
            .collect();
        function.next().unwrap()
    } else {
        auto_define_list
    };

    let mut body = Vec::new();
    for stmt in statement_list.into_inner() {
        parse_stmt(stmt, true, false, &mut body)?;
    }
    Ok(Function {
        name,
        parameters: parameters.into(),
        locals: locals.into(),
        body: body.into(),
    })
}

#[derive(pest_derive::Parser)]
#[grammar = "bc_util/grammar.pest"]
pub struct BcParser;

pub type Program = Vec<StmtInstruction>;

pub type PestError = pest::error::Error<Rule>;

fn location_end(loc: &InputLocation) -> usize {
    match loc {
        InputLocation::Pos(p) => *p,
        InputLocation::Span((_, end)) => *end,
    }
}

fn is_incomplete(text: &str, error: &PestError) -> bool {
    let pos = location_end(&error.location);
    // The program is incomplete if either:
    // - we expect something after the end of the input
    // - the error occurs at the start of and incomplete comment
    // - the error occurs at the start of an incomplete string
    pos == text.len()
        || text.as_bytes()[pos..text.len().min(pos + 2)] == [b'/', b'*']
        || text.as_bytes()[pos] == b'"'
}

fn improve_pest_error(err: PestError, file_path: Option<&str>) -> PestError {
    let err = if let Some(path) = file_path {
        err.with_path(path)
    } else {
        err
    };
    err.renamed_rules(|rule| {
        match *rule {
            Rule::add => "'+'",
            Rule::sub => "'-'",
            Rule::mul => "'*'",
            Rule::div => "'/'",
            Rule::modulus => "'%'",
            Rule::pow => "'^'",
            Rule::neg => "'-'",
            Rule::assign => "'='",
            Rule::add_assign => "'+='",
            Rule::sub_assign => "'-='",
            Rule::mul_assign => "'*='",
            Rule::div_assign => "'/='",
            Rule::mod_assign => "'%='",
            Rule::pow_assign => "'^='",
            Rule::same => "'=='",
            Rule::neq => "'!='",
            Rule::lt => "'<'",
            Rule::leq => "'<='",
            Rule::gt => "'>'",
            Rule::geq => "'>='",
            Rule::primary => "expression",
            _ => return format!("{:?}", rule),
        }
        .to_string()
    })
}

#[derive(Debug)]
pub struct ParseError {
    err: PestError,
    /// is `true` if the parsed program contains an incomplete expression,
    /// statement, comment or string.
    /// # Examples
    /// ```
    /// assert!(parse_program("1 + 2 *\\\n").unwrap_err().is_incomplete)
    /// assert!(parse_program("define f() {\n").unwrap_err().is_incomplete)
    /// assert!(parse_program("if (c) {\n").unwrap_err().is_incomplete)
    /// assert!(parse_program("while (c) {\n").unwrap_err().is_incomplete)
    /// ```
    pub is_incomplete: bool,
}

impl From<PestError> for ParseError {
    fn from(err: PestError) -> Self {
        ParseError {
            err,
            is_incomplete: false,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

impl ParseError {
    fn new(text: &str, file_path: Option<&str>, err: PestError) -> Self {
        let is_incomplete = is_incomplete(text, &err);
        let err = improve_pest_error(err, file_path);
        ParseError { err, is_incomplete }
    }
}

pub fn parse_program(text: &str, file_path: Option<&str>) -> Result<Program, ParseError> {
    let program = BcParser::parse(Rule::program, text)
        .map_err(|e| ParseError::new(text, file_path, e))?
        .next()
        .unwrap();
    let mut result = Vec::new();
    for item in program.into_inner() {
        match item.as_rule() {
            Rule::semicolon_list => {
                // stmt*
                for stmt in item.into_inner() {
                    parse_stmt(stmt, false, false, &mut result)?;
                }
            }
            Rule::function => {
                let f = parse_function(item)?;
                result.push(StmtInstruction::DefineFunction {
                    name: f.name,
                    function: f,
                });
            }
            Rule::EOI => {}
            _ => unreachable!(),
        }
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_expr(input: &str) -> ExprInstruction {
        let program = parse_program(input, None).expect("error parsing expression");
        assert_eq!(program.len(), 1);
        if let StmtInstruction::Expr(expr) = program.into_iter().next().unwrap() {
            expr
        } else {
            panic!("expected expression")
        }
    }

    fn parse_stmt(input: &str) -> StmtInstruction {
        let program = parse_program(input, None).expect("error parsing statement");
        assert_eq!(program.len(), 1);
        program.into_iter().next().unwrap()
    }

    fn parse_function(input: &str) -> Function {
        let program = parse_program(input, None).expect("error parsing function");
        assert_eq!(program.len(), 1);
        if let StmtInstruction::DefineFunction { function, .. } =
            program.into_iter().next().unwrap()
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
        let instructions = parse_program("", None).expect("error parsing empty program");
        assert_eq!(instructions.len(), 0);
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
        let expr = parse_expr("length(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Length,
                arg: Box::new(ExprInstruction::Number("123".to_string()))
            })
        );
        let expr = parse_expr("sqrt(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Sqrt,
                arg: Box::new(ExprInstruction::Number("123".to_string()))
            })
        );
        let expr = parse_expr("scale(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Scale,
                arg: Box::new(ExprInstruction::Number("123".to_string()))
            })
        );
    }

    #[test]
    fn test_parse_pre_increment() {
        let expr = parse_expr("++d\n");
        assert_eq!(
            expr,
            ExprInstruction::PreIncrement(NamedExpr::VariableNumber('d'))
        );
    }

    #[test]
    fn test_parse_pre_decrement() {
        let expr = parse_expr("--g\n");
        assert_eq!(
            expr,
            ExprInstruction::PreDecrement(NamedExpr::VariableNumber('g'))
        );
    }

    #[test]
    fn test_parse_post_increment() {
        let expr = parse_expr("e++\n");
        assert_eq!(
            expr,
            ExprInstruction::PostIncrement(NamedExpr::VariableNumber('e'))
        );
    }

    #[test]
    fn test_parse_post_decrement() {
        let expr = parse_expr("f--\n");
        assert_eq!(
            expr,
            ExprInstruction::PostDecrement(NamedExpr::VariableNumber('f'))
        );
    }

    #[test]
    fn test_parse_fn_call_no_args() {
        let expr = parse_expr("a()\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'a',
                args: vec![]
            }
        );
    }

    #[test]
    fn test_parse_fn_one_arg() {
        let expr = parse_expr("a(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'a',
                args: vec![FunctionArgument::Expr(ExprInstruction::Number(
                    "1".to_string()
                )),]
            }
        );
    }

    #[test]
    fn test_parse_fn_multiple_args() {
        let expr = parse_expr("a(1, a, b[])\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'a',
                args: vec![
                    FunctionArgument::Expr(ExprInstruction::Number("1".to_string())),
                    FunctionArgument::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    FunctionArgument::ArrayVariable('b')
                ]
            }
        );
    }

    #[test]
    fn test_parse_register_assignment() {
        let expr = parse_expr("scale = 10\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::Scale,
                value: Box::new(ExprInstruction::Number("10".to_string()))
            }
        );
        let expr = parse_expr("ibase = 16\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::IBase,
                value: Box::new(ExprInstruction::Number("16".to_string()))
            }
        );
        let expr = parse_expr("obase = 2\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::OBase,
                value: Box::new(ExprInstruction::Number("2".to_string()))
            }
        );
    }

    #[test]
    fn test_parse_register_compound_assignment() {
        let expr = parse_expr("scale += 10\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::Scale,
                value: Box::new(ExprInstruction::Add(
                    Box::new(ExprInstruction::GetRegister(Register::Scale)),
                    Box::new(ExprInstruction::Number("10".to_string()))
                ))
            }
        );
        let expr = parse_expr("ibase -= 16\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::IBase,
                value: Box::new(ExprInstruction::Sub(
                    Box::new(ExprInstruction::GetRegister(Register::IBase)),
                    Box::new(ExprInstruction::Number("16".to_string()))
                ))
            }
        );
        let expr = parse_expr("obase *= 2\n");
        assert_eq!(
            expr,
            ExprInstruction::SetRegister {
                register: Register::OBase,
                value: Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::GetRegister(Register::OBase)),
                    Box::new(ExprInstruction::Number("2".to_string()))
                ))
            }
        );
    }

    #[test]
    fn test_parse_simple_variable_assignment() {
        let expr = parse_expr("a = 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Number("1".to_string()))
            })
        );
    }

    #[test]
    fn test_parse_assign_to_array_element() {
        let expr = parse_expr("a[20] = 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::ArrayItem {
                    name: 'a',
                    index: Box::new(ExprInstruction::Number("20".to_string())),
                },
                value: Box::new(ExprInstruction::Number("1".to_string()))
            })
        );
    }

    #[test]
    fn test_parse_compound_variable_assignment() {
        let expr = parse_expr("a += 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Add(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
        );
        let expr = parse_expr("a -= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Sub(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
        );
        let expr = parse_expr("a *= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
        );
        let expr = parse_expr("a /= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Div(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
        );
        let expr = parse_expr("a %= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Mod(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
        );
        let expr = parse_expr("a ^= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                named: NamedExpr::VariableNumber('a'),
                value: Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number("1".to_string()))
                ))
            })
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
    fn test_parse_binary_op() {
        let expr = parse_expr("1 + 2\n");
        assert_eq!(
            expr,
            (ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
        let expr = parse_expr("1 ^ 2\n");
        assert_eq!(
            expr,
            (ExprInstruction::Pow(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
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
        let expr = parse_expr("1 * 2 + 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Number("1".to_string())),
                    Box::new(ExprInstruction::Number("2".to_string()))
                )),
                Box::new(ExprInstruction::Number("3".to_string()))
            )
        );
        let expr = parse_expr("1 * 2 ^ 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Mul(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Number("2".to_string())),
                    Box::new(ExprInstruction::Number("3".to_string()))
                ))
            )
        );
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
        let expr = parse_expr("1 ^ 2 ^ 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Pow(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Number("2".to_string())),
                    Box::new(ExprInstruction::Number("3".to_string()))
                ))
            )
        );
    }

    #[test]
    fn test_parse_break() {
        let stmt = parse_stmt("while(0) break\n");
        assert_eq!(
            stmt,
            StmtInstruction::While {
                condition: ConditionInstruction::Expr(ExprInstruction::Number("0".to_string())),
                body: vec![StmtInstruction::Break]
            }
        );
    }

    #[test]
    fn test_parse_quit() {
        let stmt = parse_stmt("quit\n");
        assert_eq!(stmt, StmtInstruction::Quit);
    }

    #[test]
    fn test_parse_empty_return() {
        let stmt = parse_stmt("define f() {\nreturn\n}\n");
        assert_eq!(
            stmt,
            StmtInstruction::DefineFunction {
                name: 'f',
                function: Function {
                    name: 'f',
                    body: [StmtInstruction::Return].into(),
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn test_parse_return_expr() {
        let stmt = parse_stmt("define f() {\nreturn(1)\n}\n");
        assert_eq!(
            stmt,
            StmtInstruction::DefineFunction {
                name: 'f',
                function: Function {
                    name: 'f',
                    body: [StmtInstruction::ReturnExpr(ExprInstruction::Number(
                        "1".to_string()
                    ))]
                    .into(),
                    ..Default::default()
                }
            }
        );
    }

    #[test]
    fn test_parse_if() {
        let stmt = parse_stmt("if (x <= z) a = 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::If {
                condition: ConditionInstruction::Leq(
                    ExprInstruction::Named(NamedExpr::VariableNumber('x')),
                    ExprInstruction::Named(NamedExpr::VariableNumber('z'))
                ),
                body: vec![StmtInstruction::Expr(ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('a'),
                    value: Box::new(ExprInstruction::Number("2".to_string()))
                })]
            }
        );
    }

    #[test]
    fn test_parse_while() {
        let stmt = parse_stmt("while (x <= z) a = 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::While {
                condition: ConditionInstruction::Leq(
                    ExprInstruction::Named(NamedExpr::VariableNumber('x')),
                    ExprInstruction::Named(NamedExpr::VariableNumber('z'))
                ),
                body: vec![StmtInstruction::Expr(ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('a'),
                    value: Box::new(ExprInstruction::Number("2".to_string()))
                })]
            }
        );
    }

    #[test]
    fn test_parse_for() {
        let stmt = parse_stmt("for (i = 0; i < 10; i++) a = 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::For {
                init: ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('i'),
                    value: Box::new(ExprInstruction::Number("0".to_string()))
                },
                condition: ConditionInstruction::Lt(
                    ExprInstruction::Named(NamedExpr::VariableNumber('i')),
                    ExprInstruction::Number("10".to_string())
                ),
                update: ExprInstruction::PostIncrement(NamedExpr::VariableNumber('i')),
                body: vec![StmtInstruction::Expr(ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('a'),
                    value: Box::new(ExprInstruction::Number("2".to_string()))
                })]
            }
        );
    }

    #[test]
    fn test_parse_empty_braced_statement_list() {
        let instructions =
            parse_program("{ }\n", None).expect("error parsing empty braced statement list");
        assert_eq!(instructions.len(), 0);
        let instructions =
            parse_program("{\n}\n", None).expect("error parsing empty braced statement list");
        assert_eq!(instructions.len(), 0);
        let instructions =
            parse_program("{\n\n}\n", None).expect("error parsing empty braced statement list");
        assert_eq!(instructions.len(), 0);
        let instructions =
            parse_program("{;\n;;}\n", None).expect("error parsing empty braced statement list");
        assert_eq!(instructions.len(), 0);
    }

    #[test]
    fn test_parse_braced_statement_list() {
        let instructions = parse_program("{ 1 + 2; 3 + 4; \"string\" }\n", None)
            .expect("error parsing braced statement list");
        assert_eq!(instructions.len(), 3);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("3".to_string())),
                Box::new(ExprInstruction::Number("4".to_string()))
            ))
        );
        assert_eq!(
            instructions[2],
            StmtInstruction::String("string".to_string())
        );
    }

    #[test]
    fn test_parse_string() {
        let stmt = parse_stmt("\"hello\"\n");
        assert_eq!(stmt, StmtInstruction::String("hello".to_string()));
    }

    #[test]
    fn test_parse_multiline_string() {
        let stmt = parse_stmt("\"hello\nworld\"\n");
        assert_eq!(stmt, StmtInstruction::String("hello\nworld".to_string()));
    }

    #[test]
    fn test_parse_expression() {
        let stmt = parse_stmt("1 + 2\n");
        assert_eq!(
            stmt,
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
    }

    #[test]
    fn test_parse_multiple_statements_on_single_line() {
        let instructions = parse_program("1 + 2; 3 + 4; \"string\"\n", None)
            .expect("error parsing multiple statements");
        assert_eq!(instructions.len(), 3);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("3".to_string())),
                Box::new(ExprInstruction::Number("4".to_string()))
            ))
        );
        assert_eq!(
            instructions[2],
            StmtInstruction::String("string".to_string())
        );
    }

    #[test]
    fn test_parse_function_empty_no_parameters() {
        let func = parse_function("define f() {\n }\n");
        assert_eq!(
            func,
            Function {
                name: 'f',
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_parse_function_empty_with_parameters() {
        let func = parse_function("define f(a[], b, c, d[]) {\n }\n");
        assert_eq!(
            func,
            Function {
                name: 'f',
                parameters: [
                    Variable::Array('a'),
                    Variable::Number('b'),
                    Variable::Number('c'),
                    Variable::Array('d')
                ]
                .into(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_parse_function_with_locals() {
        let func = parse_function("define f() {\n auto a[], b, c, d[]\n}\n");
        assert_eq!(
            func,
            Function {
                name: 'f',
                locals: [
                    Variable::Array('a'),
                    Variable::Number('b'),
                    Variable::Number('c'),
                    Variable::Array('d')
                ]
                .into(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_parse_function_with_statements() {
        let func = parse_function("define f() {\n 1 + 2\n}\n");
        assert_eq!(
            func,
            Function {
                name: 'f',
                body: [StmtInstruction::Expr(ExprInstruction::Add(
                    Box::new(ExprInstruction::Number("1".to_string())),
                    Box::new(ExprInstruction::Number("2".to_string()))
                ))]
                .into(),
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_ignore_comments() {
        let instructions = parse_program(
            "/*line comment*/\n1 + 2; \n/*multiline\ncomment*/\n3 + 4\n",
            None,
        )
        .expect("error parsing multiple statements with comments");
        assert_eq!(instructions.len(), 2);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("3".to_string())),
                Box::new(ExprInstruction::Number("4".to_string()))
            ))
        );
    }

    #[test]
    fn test_ignore_backslash_newline() {
        let stmt = parse_stmt("1 + \\\n2\n");
        assert_eq!(
            stmt,
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            ))
        );
    }

    #[test]
    fn test_break_outside_of_loop_is_an_error() {
        let result = parse_program("break\n", None);
        assert!(result.is_err());
    }

    #[test]
    fn test_return_outside_of_function_is_an_error() {
        let result = parse_program("return\n", None);
        assert!(result.is_err());
    }

    #[test]
    fn test_partial_comment_is_incomplete() {
        assert!(program_err("/* this is the start of a comment\n").is_incomplete);
        assert!(program_err("a + /* this is the start of a comment\n").is_incomplete);
        assert!(program_err("1 + 2;/* this is the start of a comment\n").is_incomplete);
    }

    #[test]
    fn test_partial_string_is_incomplete() {
        assert!(program_err("\"this is the start of a string\n").is_incomplete);
        assert!(program_err("1 + 2;\"this is the start of a string\n").is_incomplete);
    }

    #[test]
    fn test_partial_function_requires_is_incomplete() {
        assert!(program_err("define f() {\n").is_incomplete);
        assert!(program_err("define f() {\n auto a[], b, c, d[]\n").is_incomplete);
        assert!(program_err("define f() {\n 1 + 2\n").is_incomplete);
        assert!(program_err("define f() {\n auto a, b, c[];\n1 + 2;\n").is_incomplete);
    }

    #[test]
    fn test_unclosed_braced_statement_list_is_incomplete() {
        assert!(program_err("{\n").is_incomplete);
        assert!(program_err("{ 1 + 2; 3 + 4; \"string\"\n").is_incomplete);
    }

    #[test]
    fn test_statements_ending_with_a_backslash_newline_are_incomplete() {
        assert!(program_err("1 + 2 + \\\n").is_incomplete);
        assert!(program_err("1 + 2\\\n\\\n").is_incomplete);
    }

    #[test]
    fn test_parse_parenthesized_expression() {
        let expr = parse_expr("(1 + 2)\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number("1".to_string())),
                Box::new(ExprInstruction::Number("2".to_string()))
            )
        );
    }
}
