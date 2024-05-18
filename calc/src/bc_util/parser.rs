//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};

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

fn parse_named_expr(expr: Pair<Rule>) -> NamedExpr {
    let expr = first_child(expr);
    match expr.as_rule() {
        Rule::scale => NamedExpr::Scale,
        Rule::ibase => NamedExpr::IBase,
        Rule::obase => NamedExpr::OBase,
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

fn parse_primary(expr: Pair<Rule>) -> ExprInstruction {
    let expr = expr.into_inner().next().unwrap();
    match expr.as_rule() {
        Rule::number => ExprInstruction::Number(expr.as_str().trim().parse().unwrap()),
        Rule::paren => parse_expr(expr),
        Rule::builtin_call => {
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
            let args = inner.next().unwrap().into_inner().map(parse_expr).collect();
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
        Rule::negation => ExprInstruction::UnaryMinus(Box::new(parse_expr(first_child(expr)))),
        Rule::assignment => {
            // name assignment_operator expr
            let mut inner = expr.into_inner();
            let name = as_letter(inner.next().unwrap());
            let op = inner.next().unwrap();
            let value = parse_expr(inner.next().unwrap());
            match op.as_str() {
                "=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(value),
                },
                "+=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Add(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                "-=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Sub(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                "*=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Mul(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                "/=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Div(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                "%=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Mod(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                "^=" => ExprInstruction::Assignment {
                    name,
                    value: Box::new(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Named(NamedExpr::VariableNumber(name))),
                        Box::new(value),
                    )),
                },
                _ => unreachable!(),
            }
        }
        Rule::named_expression => ExprInstruction::Named(parse_named_expr(expr)),
        _ => unreachable!(),
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
            match op.as_str() {
                "==" => ConditionInstruction::Eq(left, right),
                "!=" => ConditionInstruction::Ne(left, right),
                "<" => ConditionInstruction::Lt(left, right),
                "<=" => ConditionInstruction::Leq(left, right),
                ">" => ConditionInstruction::Gt(left, right),
                ">=" => ConditionInstruction::Geq(left, right),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn parse_stmt(stmt: Pair<Rule>, statements: &mut Vec<StmtInstruction>) {
    let stmt = first_child(stmt);
    match stmt.as_rule() {
        Rule::break_stmt => {
            statements.push(StmtInstruction::Break);
        }
        Rule::quit => {
            statements.push(StmtInstruction::Quit);
        }
        Rule::return_stmt => {
            let mut inner = stmt.into_inner();
            if let Some(expr) = inner.next() {
                statements.push(StmtInstruction::ReturnExpr(parse_expr(expr)));
            } else {
                statements.push(StmtInstruction::Return);
            }
        }
        Rule::if_stmt => {
            let mut inner = stmt.into_inner();
            let condition = parse_condition(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), &mut body);
            statements.push(StmtInstruction::If { condition, body });
        }
        Rule::while_stmt => {
            let mut inner = stmt.into_inner();
            let condition = parse_condition(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), &mut body);
            statements.push(StmtInstruction::While { condition, body });
        }
        Rule::for_stmt => {
            let mut inner = stmt.into_inner();
            let init = parse_expr(inner.next().unwrap());
            statements.push(StmtInstruction::Expr(init));
            let condition = parse_condition(inner.next().unwrap());
            let after = parse_expr(inner.next().unwrap());
            let mut body = Vec::new();
            parse_stmt(inner.next().unwrap(), &mut body);
            body.push(StmtInstruction::Expr(after));
            statements.push(StmtInstruction::While { condition, body });
        }

        Rule::scoped_statement_list => {
            for stmt in first_child(stmt).into_inner() {
                parse_stmt(stmt, statements);
            }
        }
        Rule::string => {
            statements.push(StmtInstruction::String(
                stmt.as_str().trim_matches('\"').to_string(),
            ));
        }
        Rule::expression => {
            statements.push(StmtInstruction::Expr(parse_expr(stmt)));
        }
        _ => unreachable!(),
    }
}

fn parse_function(func: Pair<Rule>) -> Function {
    let mut function = func.into_inner();
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
        parse_stmt(stmt, &mut body);
    }
    Function {
        name,
        parameters,
        locals,
        body,
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "bc_util/grammar.pest"]
pub struct BcParser;

pub type Program = Vec<StmtInstruction>;

pub fn parse_program(text: &str) -> Result<Program, pest::error::Error<Rule>> {
    let program = BcParser::parse(Rule::program, text)?.next().unwrap();
    let mut result = Vec::new();
    let input_items = first_child(program);
    for item in input_items.into_inner() {
        match item.as_rule() {
            Rule::semicolon_list => {
                for stmt in item.into_inner() {
                    parse_stmt(stmt, &mut result);
                }
            }
            Rule::function => {
                let f = parse_function(item);
                result.push(StmtInstruction::DefineFunction {
                    name: f.name,
                    function: f,
                });
            }
            _ => unreachable!(),
        }
    }
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_expr(input: &str) -> ExprInstruction {
        let program = parse_program(input).expect("error parsing expression");
        assert_eq!(program.len(), 1);
        if let StmtInstruction::Expr(expr) = program.into_iter().next().unwrap() {
            expr
        } else {
            panic!("expected expression")
        }
    }

    fn parse_stmt(input: &str) -> StmtInstruction {
        let program = parse_program(input).expect("error parsing statement");
        assert_eq!(program.len(), 1);
        program.into_iter().next().unwrap()
    }

    fn parse_function(input: &str) -> Function {
        let program = parse_program(input).expect("error parsing function");
        assert_eq!(program.len(), 1);
        if let StmtInstruction::DefineFunction { function, .. } =
            program.into_iter().next().unwrap()
        {
            function
        } else {
            panic!("expected function")
        }
    }

    #[test]
    fn test_parse_number() {
        let expr = parse_expr("123\n");
        assert_eq!(expr, ExprInstruction::Number(123.0));
        let expr = parse_expr("123.456\n");
        assert_eq!(expr, ExprInstruction::Number(123.456));
        let expr = parse_expr(".456\n");
        assert_eq!(expr, ExprInstruction::Number(0.456));
        let expr = parse_expr("123.\n");
        assert_eq!(expr, ExprInstruction::Number(123.0));
    }

    #[test]
    fn test_parse_named() {
        let expr = parse_expr("scale\n");
        assert_eq!(expr, ExprInstruction::Named(NamedExpr::Scale));
        let expr = parse_expr("ibase\n");
        assert_eq!(expr, ExprInstruction::Named(NamedExpr::IBase));
        let expr = parse_expr("obase\n");
        assert_eq!(expr, ExprInstruction::Named(NamedExpr::OBase));
        let expr = parse_expr("a\n");
        assert_eq!(expr, ExprInstruction::Named(NamedExpr::VariableNumber('a')));
        let expr = parse_expr("a[1]\n");
        assert_eq!(
            expr,
            (ExprInstruction::Named(NamedExpr::ArrayItem {
                name: 'a',
                index: Box::new(ExprInstruction::Number(1.0))
            }))
        );
    }

    #[test]
    fn test_parse_builtin_call() {
        let expr = parse_expr("length(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Length,
                arg: Box::new(ExprInstruction::Number(123.0))
            })
        );
        let expr = parse_expr("sqrt(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Sqrt,
                arg: Box::new(ExprInstruction::Number(123.0))
            })
        );
        let expr = parse_expr("scale(123)\n");
        assert_eq!(
            expr,
            (ExprInstruction::Builtin {
                function: BuiltinFunction::Scale,
                arg: Box::new(ExprInstruction::Number(123.0))
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
    fn test_parse_fn_call() {
        let expr = parse_expr("a(1)\n");
        assert_eq!(
            expr,
            ExprInstruction::Call {
                name: 'a',
                args: vec![ExprInstruction::Number(1.0),]
            }
        );
    }

    #[test]
    fn test_parse_simple_assignment() {
        let expr = parse_expr("a = 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Number(1.0))
            })
        );
    }

    #[test]
    fn test_parse_compound_assignment() {
        let expr = parse_expr("a += 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Add(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number(1.0))
                ))
            })
        );
        let expr = parse_expr("a -= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Sub(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number(1.0))
                ))
            })
        );
        let expr = parse_expr("a *= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number(1.0))
                ))
            })
        );
        let expr = parse_expr("a /= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Div(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number(1.0))
                ))
            })
        );
        let expr = parse_expr("a ^= 1\n");
        assert_eq!(
            expr,
            (ExprInstruction::Assignment {
                name: 'a',
                value: Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                    Box::new(ExprInstruction::Number(1.0))
                ))
            })
        );
    }

    #[test]
    fn test_parse_unary_minus() {
        let expr = parse_expr("-1\n");
        assert_eq!(
            expr,
            ExprInstruction::UnaryMinus(Box::new(ExprInstruction::Number(1.0)))
        );
    }

    #[test]
    fn test_parse_binary_op() {
        let expr = parse_expr("1 + 2\n");
        assert_eq!(
            expr,
            (ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
        let expr = parse_expr("1 ^ 2\n");
        assert_eq!(
            expr,
            (ExprInstruction::Pow(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
    }

    #[test]
    fn test_parse_correct_precedence() {
        let expr = parse_expr("1 + 2 * 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Number(2.0)),
                    Box::new(ExprInstruction::Number(3.0))
                ))
            )
        );
        let expr = parse_expr("1 * 2 + 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Add(
                Box::new(ExprInstruction::Mul(
                    Box::new(ExprInstruction::Number(1.0)),
                    Box::new(ExprInstruction::Number(2.0))
                )),
                Box::new(ExprInstruction::Number(3.0))
            )
        );
        let expr = parse_expr("1 * 2 ^ 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Mul(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Number(2.0)),
                    Box::new(ExprInstruction::Number(3.0))
                ))
            )
        );
    }

    #[test]
    fn test_pow_is_right_associative() {
        let expr = parse_expr("1 ^ 2 ^ 3\n");
        assert_eq!(
            expr,
            ExprInstruction::Pow(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Pow(
                    Box::new(ExprInstruction::Number(2.0)),
                    Box::new(ExprInstruction::Number(3.0))
                ))
            )
        );
    }

    #[test]
    fn test_parse_break() {
        let stmt = parse_stmt("break\n");
        assert_eq!(stmt, StmtInstruction::Break);
    }

    #[test]
    fn test_parse_quit() {
        let stmt = parse_stmt("quit\n");
        assert_eq!(stmt, StmtInstruction::Quit);
    }

    #[test]
    fn test_parse_empty_return() {
        let stmt = parse_stmt("return\n");
        assert_eq!(stmt, StmtInstruction::Return);
    }

    #[test]
    fn test_parse_return_expr() {
        let stmt = parse_stmt("return(1)\n");
        assert_eq!(
            stmt,
            StmtInstruction::ReturnExpr(ExprInstruction::Number(1.0))
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
                    name: 'a',
                    value: Box::new(ExprInstruction::Number(2.0))
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
                    name: 'a',
                    value: Box::new(ExprInstruction::Number(2.0))
                })]
            }
        );
    }

    #[test]
    fn test_parse_for() {
        let instructions =
            parse_program("for (i = 0; i < 10; i++) a = 2\n").expect("error parsing for loop");
        assert_eq!(instructions.len(), 2);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Assignment {
                name: 'i',
                value: Box::new(ExprInstruction::Number(0.0))
            })
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::While {
                condition: ConditionInstruction::Lt(
                    ExprInstruction::Named(NamedExpr::VariableNumber('i')),
                    ExprInstruction::Number(10.0)
                ),
                body: vec![
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        name: 'a',
                        value: Box::new(ExprInstruction::Number(2.0))
                    }),
                    StmtInstruction::Expr(ExprInstruction::PostIncrement(
                        NamedExpr::VariableNumber('i')
                    ))
                ]
            }
        );
    }

    #[test]
    fn test_parse_empty_scoped_statement_list() {
        let instructions =
            parse_program("{ }\n").expect("error parsing empty scoped statement list");
        assert_eq!(instructions.len(), 0);
    }

    #[test]
    fn test_parse_scoped_statement_list() {
        let instructions = parse_program("{ 1 + 2; 3 + 4; \"string\" }\n")
            .expect("error parsing scoped statement list");
        assert_eq!(instructions.len(), 3);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(3.0)),
                Box::new(ExprInstruction::Number(4.0))
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
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
    }

    #[test]
    fn test_parse_multiple_statements_on_single_line() {
        let instructions =
            parse_program("1 + 2; 3 + 4; \"string\"\n").expect("error parsing multiple statements");
        assert_eq!(instructions.len(), 3);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(3.0)),
                Box::new(ExprInstruction::Number(4.0))
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
                parameters: vec![
                    Variable::Array('a'),
                    Variable::Number('b'),
                    Variable::Number('c'),
                    Variable::Array('d')
                ],
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
                locals: vec![
                    Variable::Array('a'),
                    Variable::Number('b'),
                    Variable::Number('c'),
                    Variable::Array('d')
                ],
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
                body: vec![StmtInstruction::Expr(ExprInstruction::Add(
                    Box::new(ExprInstruction::Number(1.0)),
                    Box::new(ExprInstruction::Number(2.0))
                ))],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_ignore_comments() {
        let instructions = parse_program("1 + 2; /*multiline\ncomment*/3 + 4\n")
            .expect("error parsing multiple statements with comments");
        assert_eq!(instructions.len(), 2);
        assert_eq!(
            instructions[0],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
        assert_eq!(
            instructions[1],
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(3.0)),
                Box::new(ExprInstruction::Number(4.0))
            ))
        );
    }

    #[test]
    fn test_ignore_backslash_newline() {
        let stmt = parse_stmt("1 + \\\n2\n");
        assert_eq!(
            stmt,
            StmtInstruction::Expr(ExprInstruction::Add(
                Box::new(ExprInstruction::Number(1.0)),
                Box::new(ExprInstruction::Number(2.0))
            ))
        );
    }
}
