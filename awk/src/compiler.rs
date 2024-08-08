//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::program::{
    AwkRule, BuiltinFunction, Constant, Function, OpCode, Pattern, Program, SpecialVar, VarId,
};
use crate::regex::Regex;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Parser,
};
use std::ffi::CString;
use std::rc::Rc;
use std::str::Chars;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    hash::Hash,
    str::FromStr,
};

struct BuiltinFunctionInfo {
    function: BuiltinFunction,
    min_args: u16,
    max_args: u16,
}

lazy_static::lazy_static! {
    static ref BUILTIN_FUNCTIONS: HashMap<Rule, BuiltinFunctionInfo> = HashMap::from([
        (Rule::atan2, BuiltinFunctionInfo {
            function: BuiltinFunction::Atan2,
            min_args: 2,
            max_args: 2,
        }),
        (Rule::cos, BuiltinFunctionInfo {
            function: BuiltinFunction::Cos,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::sin, BuiltinFunctionInfo {
            function: BuiltinFunction::Sin,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::exp, BuiltinFunctionInfo {
            function: BuiltinFunction::Exp,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::log, BuiltinFunctionInfo {
            function: BuiltinFunction::Log,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::sqrt, BuiltinFunctionInfo {
            function: BuiltinFunction::Sqrt,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::int, BuiltinFunctionInfo {
            function: BuiltinFunction::Int,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::rand, BuiltinFunctionInfo {
            function: BuiltinFunction::Rand,
            min_args: 0,
            max_args: 0,
        }),
        (Rule::srand, BuiltinFunctionInfo {
            function: BuiltinFunction::Srand,
            min_args: 0,
            max_args: 1,
        }),

        (Rule::gsub, BuiltinFunctionInfo {
            function: BuiltinFunction::Gsub,
            min_args: 2,
            max_args: 3,
        }),
        (Rule::index, BuiltinFunctionInfo {
            function: BuiltinFunction::Index,
            min_args: 2,
            max_args: 2,
        }),
        (Rule::length, BuiltinFunctionInfo {
            function: BuiltinFunction::Length,
            min_args: 0,
            max_args: 1,
        }),
        (Rule::match_fn, BuiltinFunctionInfo {
            function: BuiltinFunction::Match,
            min_args: 2,
            max_args: 2,
        }),
        (Rule::split, BuiltinFunctionInfo {
            function: BuiltinFunction::Split,
            min_args: 2,
            max_args: 3,
        }),
        (Rule::sprintf, BuiltinFunctionInfo {
            function: BuiltinFunction::Sprintf,
            min_args: 1,
            max_args: u16::MAX,
        }),
        (Rule::sub, BuiltinFunctionInfo {
            function: BuiltinFunction::Sub,
            min_args: 2,
            max_args: 3,
        }),
        (Rule::substr, BuiltinFunctionInfo {
            function: BuiltinFunction::Substr,
            min_args: 2,
            max_args: 3,
        }),
        (Rule::tolower, BuiltinFunctionInfo {
            function: BuiltinFunction::ToLower,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::toupper, BuiltinFunctionInfo {
            function: BuiltinFunction::ToUpper,
            min_args: 1,
            max_args: 1,
        }),

        (Rule::close, BuiltinFunctionInfo {
            function: BuiltinFunction::Close,
            min_args: 1,
            max_args: 1,
        }),
        (Rule::system, BuiltinFunctionInfo {
            function: BuiltinFunction::System,
            min_args: 1,
            max_args: 1,
        })
    ]);
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc, Op};

        // Precedence is defined lowest to highest
        PrattParser::new()
        .op(Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::in_op, Assoc::Left))
        .op(Op::infix(Rule::match_op, Assoc::Left)
            | Op::infix(Rule::not_match, Assoc::Left))
        .op(Op::infix(Rule::comp_op, Assoc::Left))
        .op(Op::infix(Rule::concat, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left)
            | Op::infix(Rule::binary_sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left)
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::modulus, Assoc::Left))
        .op(Op::prefix(Rule::not)
            | Op::prefix(Rule::negate)
            | Op::prefix(Rule::unary_plus))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::prefix(Rule::pre_inc)
            | Op::prefix(Rule::pre_dec))
        .op(Op::postfix(Rule::post_inc)
            | Op::postfix(Rule::post_dec))
    };
}

#[derive(pest_derive::Parser, Default)]
#[grammar = "grammar.pest"]
struct AwkParser;

type PestError = pest::error::Error<Rule>;

fn pest_error_from_span(span: pest::Span, message: String) -> PestError {
    PestError::new_from_span(pest::error::ErrorVariant::CustomError { message }, span)
}

fn first_child(pair: Pair<Rule>) -> Pair<Rule> {
    pair.into_inner().next().unwrap()
}

fn distance(start: usize, end: usize) -> i32 {
    (end as i32) - (start as i32)
}

fn is_octal_digit(c: char) -> bool {
    ('0'..='7').contains(&c)
}

/// parses an escape sequence
/// # Arguments
/// - `iter`: a character iterator placed after the '\' character in an escape sequence.
/// # Returns
/// a pair containing the escaped character and the next character in the iterator
/// # Errors
/// returns an error if the escape sequence is invalid
fn parse_escape_sequence(iter: &mut Chars) -> Result<(char, Option<char>), String> {
    let mut char_after_escape_sequence = None;
    let next_char = iter.next().ok_or("invalid escape sequence".to_string())?;
    let escaped_char = match next_char {
        '"' => '"',
        '/' => '/',
        'a' => '\x07',
        'b' => '\x08',
        'f' => '\x0C',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' => '\x0B',
        '\\' => '\\',
        n if is_octal_digit(n) => {
            let mut char_code = n.to_digit(8).unwrap();
            for _ in 0..2 {
                if let Some(c) = iter.next() {
                    if is_octal_digit(c) {
                        char_code = char_code * 8 + c.to_digit(8).unwrap();
                    } else {
                        char_after_escape_sequence = Some(c);
                        break;
                    }
                }
            }
            // FIXME: I don't think this is correct. We should also consider multi-byte characters
            char::from_u32(char_code).ok_or("invalid character")?
        }
        other => return Err(format!("invalid escape sequence: \\{}", other)),
    };
    let char_after_escape_sequence = char_after_escape_sequence.or_else(|| iter.next());
    Ok((escaped_char, char_after_escape_sequence))
}

fn escape_string(s: &str) -> Result<String, String> {
    let mut result = String::new();
    let s = s.trim_matches('"');
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                let (escaped_char, next) = parse_escape_sequence(&mut chars)?;
                result.push(escaped_char);
                if let Some(next) = next {
                    result.push(next);
                }
            }
            other => result.push(other),
        }
    }
    Ok(result)
}

fn post_increment(val: &Cell<u32>) -> u32 {
    let result = val.get();
    val.set(result + 1);
    result
}

#[derive(Debug, PartialEq, Eq)]
enum ExprKind {
    LValue,
    Number,
    String,
    Regex,
    Comp,
}

struct Expr {
    kind: ExprKind,
    instructions: Vec<OpCode>,
}

impl Expr {
    fn new(kind: ExprKind, instructions: Vec<OpCode>) -> Self {
        Expr { kind, instructions }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum GlobalNameKind {
    Function,
    SpecialVar,
    Var,
}

#[derive(Clone, Copy)]
pub enum GlobalName {
    Variable(VarId),
    SpecialVar(VarId),
    Function { id: u32, parameter_count: u32 },
    BuiltinFunction,
}

type NameMap = HashMap<String, GlobalName>;
type LocalMap = HashMap<String, VarId>;

struct Compiler {
    constants: RefCell<Vec<Constant>>,
    names: RefCell<NameMap>,
    last_global_var_id: Cell<u32>,
    last_global_function_id: Cell<u32>,
    in_function: bool,
}

impl Default for Compiler {
    fn default() -> Self {
        let default_globals = HashMap::from([
            (
                "ARGC".to_string(),
                GlobalName::SpecialVar(SpecialVar::Argc as u32),
            ),
            (
                "ARGV".to_string(),
                GlobalName::SpecialVar(SpecialVar::Argv as u32),
            ),
            (
                "CONVFMT".to_string(),
                GlobalName::SpecialVar(SpecialVar::Convfmt as u32),
            ),
            (
                "ENVIRON".to_string(),
                GlobalName::SpecialVar(SpecialVar::Environ as u32),
            ),
            (
                "FILENAME".to_string(),
                GlobalName::SpecialVar(SpecialVar::Filename as u32),
            ),
            (
                "FNR".to_string(),
                GlobalName::SpecialVar(SpecialVar::Fnr as u32),
            ),
            (
                "NF".to_string(),
                GlobalName::SpecialVar(SpecialVar::Nf as u32),
            ),
            (
                "NR".to_string(),
                GlobalName::SpecialVar(SpecialVar::Nr as u32),
            ),
            (
                "OFMT".to_string(),
                GlobalName::SpecialVar(SpecialVar::Ofmt as u32),
            ),
            (
                "OFS".to_string(),
                GlobalName::SpecialVar(SpecialVar::Ofs as u32),
            ),
            (
                "ORS".to_string(),
                GlobalName::SpecialVar(SpecialVar::Ors as u32),
            ),
            (
                "RS".to_string(),
                GlobalName::SpecialVar(SpecialVar::Rs as u32),
            ),
            (
                "RSTART".to_string(),
                GlobalName::SpecialVar(SpecialVar::Rstart as u32),
            ),
            (
                "SUBSEP".to_string(),
                GlobalName::SpecialVar(SpecialVar::Subsep as u32),
            ),
        ]);
        Compiler {
            constants: RefCell::new(Vec::new()),
            names: RefCell::new(default_globals),
            last_global_var_id: Cell::new(SpecialVar::Count as u32),
            last_global_function_id: Cell::new(0),
            in_function: false,
        }
    }
}

impl Compiler {
    fn push_constant(&self, constant: Constant) -> u32 {
        let index = self.constants.borrow().len() as u32;
        self.constants.borrow_mut().push(constant);
        index
    }

    fn get_var(&self, name: &str, locals: &LocalMap) -> Result<OpCode, String> {
        // FIXME: confusing naming of var vs array
        if let Some(local_id) = locals.get(name) {
            Ok(OpCode::LocalRef(*local_id))
        } else {
            let entry = self.names.borrow().get(name).copied();
            if let Some(var) = entry {
                match var {
                    GlobalName::Variable(id) => Ok(OpCode::GlobalRef(id)),
                    GlobalName::SpecialVar(id) => Ok(OpCode::GlobalRef(id)),
                    GlobalName::Function { .. } | GlobalName::BuiltinFunction => {
                        Err(format!("'{}' function used in variable context", name))
                    }
                }
            } else {
                let id = post_increment(&self.last_global_var_id);
                self.names
                    .borrow_mut()
                    .insert(name.to_string(), GlobalName::Variable(id));
                Ok(OpCode::GlobalRef(id))
            }
        }
    }

    fn compile_function_args(
        &self,
        args: Pairs<Rule>,
        instructions: &mut Vec<OpCode>,
        call_span: pest::Span,
        locals: &LocalMap,
    ) -> Result<u16, PestError> {
        let mut argc: u16 = 0;
        for arg in args {
            self.compile_expr(arg, instructions, locals)?;
            argc += 1;
            if argc > u16::MAX {
                return Err(pest_error_from_span(
                    call_span,
                    "function call with too many arguments".to_string(),
                ));
            }
        }
        Ok(argc)
    }

    fn map_primary(&self, primary: Pair<Rule>, locals: &LocalMap) -> Result<Expr, PestError> {
        match primary.as_rule() {
            Rule::expr => {
                let mut instructions = Vec::new();
                self.compile_expr(primary, &mut instructions, locals)?;
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::ere => {
                let ere_c_str = CString::new(primary.as_str().trim_matches('/')).unwrap();
                let regex = Regex::new(ere_c_str)
                    .map_err(|e| pest_error_from_span(primary.as_span(), e))?;
                let index = self.push_constant(Constant::Regex(Rc::new(regex)));
                Ok(Expr::new(
                    ExprKind::Regex,
                    vec![OpCode::PushConstant(index)],
                ))
            }
            Rule::number => {
                // the standard rust parse doesn't fully support the awk number format
                // (C number format), so we use the parsing function provided by libc
                let number_cstr = CString::new(primary.as_str()).unwrap();
                let num = unsafe { libc::atof(number_cstr.as_ptr()) };
                let index = self.push_constant(Constant::Number(num));
                Ok(Expr::new(
                    ExprKind::Number,
                    vec![OpCode::PushConstant(index)],
                ))
            }
            Rule::string => {
                let index = self.push_constant(Constant::String(
                    escape_string(primary.as_str())
                        .map_err(|e| pest_error_from_span(primary.as_span(), e))?,
                ));
                Ok(Expr::new(
                    ExprKind::String,
                    vec![OpCode::PushConstant(index)],
                ))
            }
            Rule::lvalue => {
                let mut instructions = Vec::new();
                self.compile_lvalue(first_child(primary), &mut instructions, locals)?;
                Ok(Expr::new(ExprKind::LValue, instructions))
            }
            Rule::function_call => {
                let span = primary.as_span();
                let mut inner = primary.into_inner();
                let name = inner.next().unwrap().as_str();
                let mut instructions = Vec::new();
                let argc = self.compile_function_args(inner, &mut instructions, span, locals)?;
                match self.names.borrow().get(name) {
                    Some(GlobalName::Function {
                        id,
                        parameter_count,
                    }) => {
                        if argc > *parameter_count as u16 {
                            // TODO: other implementations simply issue a warning
                            return Err(pest_error_from_span(
                                span,
                                format!("function '{}' called with too many arguments", name),
                            ));
                        } else if argc < *parameter_count as u16 {
                            for _ in argc..*parameter_count as u16 {
                                instructions.push(OpCode::PushUninitialized);
                            }
                        }
                        instructions.push(OpCode::Call {
                            id: *id,
                            argc: *parameter_count as u16,
                        });
                    }
                    Some(_) => {
                        return Err(pest_error_from_span(
                            span,
                            format!("'{}' is not a function", name),
                        ))
                    }
                    None => {
                        return Err(pest_error_from_span(
                            span,
                            format!("call to undefined function '{}'", name),
                        ))
                    }
                }
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::builtin_function_call => {
                let span = primary.as_span();
                let mut inner = primary.into_inner();
                let function = inner.next().unwrap();
                let mut instructions = Vec::new();
                let argc = self.compile_function_args(inner, &mut instructions, span, locals)?;
                let fn_info = BUILTIN_FUNCTIONS
                    .get(&function.as_rule())
                    .expect("missing builtin");
                if (fn_info.min_args..=fn_info.max_args).contains(&argc) {
                    instructions.push(OpCode::CallBuiltin {
                        function: fn_info.function,
                        argc,
                    });
                } else {
                    return Err(pest_error_from_span(
                        span,
                        format!(
                            "incorrect number of arguments for builtin function '{}'",
                            function.as_str()
                        ),
                    ));
                }
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            _ => unreachable!(),
        }
    }

    fn map_prefix(&self, op: Pair<Rule>, rhs: Expr) -> Result<Expr, PestError> {
        let kind = rhs.kind;
        let mut instructions = rhs.instructions;
        match op.as_rule() {
            Rule::negate => {
                instructions.push(OpCode::Negate);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::not => {
                instructions.push(OpCode::Not);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::unary_plus => {
                instructions.push(OpCode::AsNumber);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::pre_inc | Rule::pre_dec => {
                if kind != ExprKind::LValue {
                    return Err(pest_error_from_span(
                        op.as_span(),
                        "operand should be an lvalue".to_string(),
                    ));
                }
                if op.as_rule() == Rule::pre_inc {
                    instructions.push(OpCode::PreInc);
                } else {
                    instructions.push(OpCode::PreDec);
                }
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            _ => unreachable!(),
        }
    }

    fn map_postfix(&self, lhs: Expr, op: Pair<Rule>) -> Result<Expr, PestError> {
        assert!(op.as_rule() == Rule::post_inc || op.as_rule() == Rule::post_dec);
        let kind = lhs.kind;
        let mut instructions = lhs.instructions;
        if kind != ExprKind::LValue {
            return Err(pest_error_from_span(
                op.as_span(),
                "operand should be an lvalue".to_string(),
            ));
        }
        if op.as_rule() == Rule::post_inc {
            instructions.push(OpCode::PostInc);
        } else {
            instructions.push(OpCode::PostDec);
        }
        Ok(Expr::new(ExprKind::Number, instructions))
    }

    fn map_infix(&self, lhs: Expr, op: Pair<Rule>, rhs: Expr) -> Result<Expr, PestError> {
        let lhs_kind = lhs.kind;
        let rhs_kind = rhs.kind;
        let mut instructions = lhs.instructions;

        match op.as_rule() {
            Rule::and => {
                instructions.push(OpCode::JumpIfFalse(rhs.instructions.len() as i32 + 1));
                instructions.extend(rhs.instructions);
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            Rule::or => {
                instructions.push(OpCode::JumpIfTrue(rhs.instructions.len() as i32 + 2));
                instructions.extend(rhs.instructions);
                instructions.push(OpCode::Jump(1));
                instructions.push(OpCode::PushOne);
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            Rule::in_op => {
                let mut lhs_instructions = instructions;
                let mut instructions = rhs.instructions;
                instructions.append(&mut lhs_instructions);
                instructions.push(OpCode::In);
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            _ => {}
        }

        instructions.extend(rhs.instructions);
        match op.as_rule() {
            Rule::add => {
                instructions.push(OpCode::Add);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::binary_sub => {
                instructions.push(OpCode::Sub);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::mul => {
                instructions.push(OpCode::Mul);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::div => {
                instructions.push(OpCode::Div);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::modulus => {
                instructions.push(OpCode::Mod);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::pow => {
                instructions.push(OpCode::Pow);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::le => {
                instructions.push(OpCode::Le);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::comp_op => {
                if lhs_kind == ExprKind::Comp || rhs_kind == ExprKind::Comp {
                    return Err(pest_error_from_span(
                        op.as_span(),
                        "cannot chain comparisons".to_string(),
                    ));
                }
                let op = first_child(op);
                instructions.push(match op.as_rule() {
                    Rule::lt => OpCode::Lt,
                    Rule::gt => OpCode::Gt,
                    Rule::le => OpCode::Le,
                    Rule::ge => OpCode::Ge,
                    Rule::eq => OpCode::Eq,
                    Rule::ne => OpCode::Ne,
                    _ => unreachable!(),
                });
                Ok(Expr::new(ExprKind::Comp, instructions))
            }
            Rule::match_op => {
                instructions.push(OpCode::Match);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::not_match => {
                instructions.push(OpCode::Match);
                instructions.push(OpCode::Not);
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::concat => {
                instructions.push(OpCode::Concat);
                Ok(Expr::new(ExprKind::String, instructions))
            }
            _ => unreachable!(),
        }
    }

    fn compile_binary_expr(&self, expr: Pairs<Rule>, locals: &LocalMap) -> Result<Expr, PestError> {
        PRATT_PARSER
            .map_primary(|primary| self.map_primary(primary, locals))
            .map_prefix(|op, rhs| self.map_prefix(op, rhs?))
            .map_postfix(|lhs, op| self.map_postfix(lhs?, op))
            .map_infix(|lhs, op, rhs| self.map_infix(lhs?, op, rhs?))
            .parse(expr)
    }

    fn compile_lvalue(
        &self,
        lvalue: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        match lvalue.as_rule() {
            Rule::name => {
                let get_instruction = self
                    .get_var(lvalue.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(lvalue.as_span(), msg))?;
                instructions.push(get_instruction);
            }
            Rule::array_element => {
                let mut inner = lvalue.into_inner();
                let name = inner.next().unwrap();
                let get_instruction = self
                    .get_var(name.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(name.as_span(), msg))?;
                instructions.push(get_instruction);
                // FIXME: only supports expression lists of one element
                let index = inner.next().unwrap();
                self.compile_expr(index, instructions, locals)?;
                instructions.push(OpCode::IndexArray)
            }
            Rule::field_var => {
                self.compile_expr(first_child(lvalue), instructions, locals)?;
                instructions.push(OpCode::FieldRef);
            }
            _ => unreachable!("encountered {:?} while compiling lvalue", lvalue.as_rule()),
        }
        Ok(())
    }

    fn compile_expr(
        &self,
        expr: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let expr = first_child(expr);
        match expr.as_rule() {
            Rule::assignment => {
                let mut inner = expr.into_inner();
                self.compile_lvalue(first_child(inner.next().unwrap()), instructions, locals)?;
                let assignment_op = first_child(inner.next().unwrap());
                if assignment_op.as_rule() != Rule::assign {
                    instructions.push(OpCode::Dup);
                    self.compile_expr(inner.next().unwrap(), instructions, locals)?;
                    match assignment_op.as_rule() {
                        Rule::add_assign => instructions.push(OpCode::Add),
                        Rule::sub_assign => instructions.push(OpCode::Sub),
                        Rule::mul_assign => instructions.push(OpCode::Mul),
                        Rule::div_assign => instructions.push(OpCode::Div),
                        Rule::mod_assign => instructions.push(OpCode::Mod),
                        Rule::pow_assign => instructions.push(OpCode::Pow),
                        _ => unreachable!(),
                    }
                } else {
                    self.compile_expr(inner.next().unwrap(), instructions, locals)?;
                }

                instructions.push(OpCode::Assign);
            }
            Rule::ternary_expr | Rule::ternary_print_expr => {
                let mut inner = expr.into_inner();
                let condition =
                    self.compile_binary_expr(inner.next().unwrap().into_inner(), locals)?;
                instructions.extend(condition.instructions);
                let mut true_expr_instructions = Vec::new();
                self.compile_expr(inner.next().unwrap(), &mut true_expr_instructions, locals)?;
                instructions.push(OpCode::JumpIfFalse(true_expr_instructions.len() as i32 + 2));
                instructions.extend(&true_expr_instructions);
                true_expr_instructions.clear();
                let mut false_expr_instructions = true_expr_instructions;
                self.compile_expr(inner.next().unwrap(), &mut false_expr_instructions, locals)?;
                instructions.push(OpCode::Jump(false_expr_instructions.len() as i32 + 1));
                instructions.extend(false_expr_instructions);
            }
            Rule::binary_expr | Rule::binary_print_expr => {
                let expr = self.compile_binary_expr(expr.into_inner(), locals)?;
                instructions.extend(expr.instructions);
            }
            Rule::input_function => todo!(),
            _ => unreachable!(
                "encountered {:?} while compiling expression",
                expr.as_rule()
            ),
        }
        Ok(())
    }

    fn compile_simple_statement(
        &mut self,
        simple_stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let stmt = first_child(simple_stmt);
        match stmt.as_rule() {
            Rule::delete_element => {
                let mut inner = stmt.into_inner();
                let name = inner.next().unwrap();
                let get_instruction = self
                    .get_var(name.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(name.as_span(), msg))?;
                instructions.push(get_instruction);
                let index = inner.next().unwrap();
                self.compile_expr(index, instructions, locals)?;
                instructions.push(OpCode::Delete);
            }
            Rule::expr => {
                self.compile_expr(stmt, instructions, locals)?;
                instructions.push(OpCode::Pop);
            }
            Rule::print_stmt => {
                let mut inner = stmt.into_inner();
                let print = inner.next().unwrap();
                match print.as_rule() {
                    Rule::simple_print | Rule::print_call => {
                        let expressions = print.into_inner();
                        let argc = expressions.len() as u16;
                        if expressions.len() == 0 {
                            todo!()
                        } else {
                            for expr in expressions {
                                self.compile_expr(expr, instructions, locals)?;
                            }
                            instructions.push(OpCode::CallBuiltin {
                                function: BuiltinFunction::Print,
                                argc,
                            });
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(
                "encountered {:?} while compiling simple statement",
                stmt.as_rule()
            ),
        }
        Ok(())
    }

    fn compile_do_while(
        &mut self,
        do_while: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = do_while.into_inner();
        let start_index = instructions.len();

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;

        let condition = inner.next().unwrap();
        self.compile_expr(condition, instructions, locals)?;
        instructions.push(OpCode::JumpIfTrue(distance(
            instructions.len(),
            start_index,
        )));

        Ok(())
    }

    fn compile_for_each(
        &mut self,
        for_each_stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        todo!();
    }

    fn compile_for(
        &mut self,
        for_stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = for_stmt.into_inner();

        let init = inner.next().unwrap();
        self.compile_simple_statement(init, instructions, locals)?;

        let condition_start = instructions.len();
        let condition = inner.next().unwrap();
        self.compile_expr(condition, instructions, locals)?;
        let for_jump_index = instructions.len();
        instructions.push(OpCode::Invalid);

        let update = inner.next().unwrap();
        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;
        self.compile_simple_statement(update, instructions, locals)?;
        instructions.push(OpCode::Jump(distance(instructions.len(), condition_start)));
        instructions[for_jump_index] =
            OpCode::JumpIfFalse(distance(for_jump_index, instructions.len()));

        Ok(())
    }

    fn compile_while(
        &mut self,
        while_stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = while_stmt.into_inner();

        let condition_start = instructions.len();
        let condition = inner.next().unwrap();
        self.compile_expr(condition, instructions, locals)?;
        let while_jump_index = instructions.len();
        instructions.push(OpCode::Invalid);

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;
        instructions.push(OpCode::Jump(distance(instructions.len(), condition_start)));

        instructions[while_jump_index] =
            OpCode::JumpIfFalse(distance(while_jump_index, instructions.len()));

        Ok(())
    }

    fn compile_if(
        &mut self,
        if_stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = if_stmt.into_inner();

        let condition = inner.next().unwrap();
        self.compile_expr(condition, instructions, locals)?;

        let if_jump_index = instructions.len();
        instructions.push(OpCode::Invalid);

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;

        if let Some(else_body) = inner.next() {
            let else_jump_index = instructions.len();
            instructions.push(OpCode::Invalid);
            instructions[if_jump_index] =
                OpCode::JumpIfFalse(distance(if_jump_index, instructions.len()));
            self.compile_stmt(else_body, instructions, locals)?;
            instructions[else_jump_index] =
                OpCode::Jump(distance(else_jump_index, instructions.len()));
        } else {
            instructions[if_jump_index] =
                OpCode::JumpIfFalse(distance(if_jump_index, instructions.len()));
        }

        Ok(())
    }

    fn compile_action(
        &mut self,
        action: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        for stmt in action.into_inner() {
            self.compile_stmt(stmt, instructions, locals)?;
        }
        Ok(())
    }

    fn compile_stmt(
        &mut self,
        stmt: Pair<Rule>,
        instructions: &mut Vec<OpCode>,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        match stmt.as_rule() {
            Rule::action => self.compile_action(stmt, instructions, locals),
            Rule::t_if => self.compile_if(stmt, instructions, locals),
            Rule::t_while => self.compile_while(stmt, instructions, locals),
            Rule::t_for => self.compile_for(stmt, instructions, locals),
            Rule::t_foreach => self.compile_for_each(stmt, instructions, locals),
            Rule::ut_if => self.compile_if(stmt, instructions, locals),
            Rule::ut_while => self.compile_while(stmt, instructions, locals),
            Rule::ut_for => self.compile_for(stmt, instructions, locals),
            Rule::ut_foreach => self.compile_for_each(stmt, instructions, locals),
            Rule::simple_statement => self.compile_simple_statement(stmt, instructions, locals),
            Rule::next => {
                instructions.push(OpCode::Next);
                Ok(())
            }
            Rule::break_stmt => todo!(),
            Rule::continue_stmt => todo!(),
            Rule::exit_stmt => {
                if let Some(expr) = stmt.into_inner().next() {
                    self.compile_expr(expr, instructions, locals)?;
                }
                instructions.push(OpCode::Exit);
                Ok(())
            }
            Rule::return_stmt => {
                if !self.in_function {
                    return Err(pest_error_from_span(
                        stmt.as_span(),
                        "return statement outside of function".to_string(),
                    ));
                }
                if let Some(expr) = stmt.into_inner().next() {
                    self.compile_expr(expr, instructions, locals)?;
                } else {
                    instructions.push(OpCode::PushUninitializedScalar);
                }
                instructions.push(OpCode::Return);
                Ok(())
            }
            Rule::do_while => self.compile_do_while(stmt, instructions, locals),
            _ => unreachable!("encountered {:?} while compiling statement", stmt.as_rule()),
        }
    }

    fn compile_normal_pattern(&mut self, pattern: Pair<Rule>) -> Result<Pattern, PestError> {
        let pattern = first_child(pattern);
        match pattern.as_rule() {
            Rule::expr => {
                let mut instructions = Vec::new();
                self.compile_expr(pattern, &mut instructions, &HashMap::new())?;
                Ok(Pattern::Expr(instructions))
            }
            Rule::range_pattern => {
                let mut inner = pattern.into_inner();

                let start = inner.next().unwrap();
                let mut start_instructions = Vec::new();
                self.compile_expr(start, &mut start_instructions, &HashMap::new())?;

                let end = inner.next().unwrap();
                let mut end_instructions = Vec::new();
                self.compile_expr(end, &mut end_instructions, &HashMap::new())?;

                Ok(Pattern::Range {
                    start: start_instructions,
                    end: end_instructions,
                })
            }
            _ => unreachable!(
                "encountered {:?} while compiling pattern",
                pattern.as_rule()
            ),
        }
    }

    fn compile_rule(&mut self, rule: Pair<Rule>) -> Result<AwkRule, PestError> {
        let rule = first_child(rule);
        match rule.as_rule() {
            Rule::action => {
                let mut instructions = Vec::new();
                self.compile_action(rule, &mut instructions, &HashMap::new())?;
                Ok(AwkRule {
                    pattern: Pattern::All,
                    instructions,
                })
            }
            Rule::pattern_and_action => {
                let mut inner = rule.into_inner();
                let pattern = self.compile_normal_pattern(inner.next().unwrap())?;
                let action = inner.next().unwrap();
                let mut instructions = Vec::new();
                let locals = HashMap::new();
                self.compile_action(action, &mut instructions, &locals)?;
                Ok(AwkRule {
                    pattern,
                    instructions,
                })
            }
            Rule::normal_pattern => {
                todo!("needs to be implemented after printing and fields are implemented");
            }
            _ => unreachable!("encountered {:?} while compiling rule", rule.as_rule()),
        }
    }

    fn compile_function_definition(&mut self, function: Pair<Rule>) -> Result<Function, PestError> {
        let mut inner = function.into_inner();
        let name = inner.next().unwrap().as_str();
        let mut param_map = HashMap::new();
        let mut parameters_count = 0;
        let maybe_param_list = inner.next().unwrap();
        let body = if maybe_param_list.as_rule() == Rule::param_list {
            for param in maybe_param_list.into_inner() {
                match self.names.get_mut().get(param.as_str()) {
                    Some(GlobalName::Function { .. }) | Some(GlobalName::SpecialVar(_)) => {
                        return Err(pest_error_from_span(
                            param.as_span(),
                            "cannot use function name or special variable as a parameter"
                                .to_string(),
                        ));
                    }
                    _ => {}
                }
                param_map.insert(param.as_str().to_string(), parameters_count as u32);
                parameters_count += 1;
            }
            inner.next().unwrap()
        } else {
            maybe_param_list
        };
        let mut instructions = Vec::new();
        self.in_function = true;
        self.compile_action(body, &mut instructions, &param_map)?;
        self.in_function = false;

        // ensure that functions always return
        if !matches!(instructions.last(), Some(OpCode::Return)) {
            instructions.push(OpCode::PushUninitializedScalar);
            instructions.push(OpCode::Return);
        }

        let id = post_increment(&self.last_global_function_id);
        self.names.get_mut().insert(
            name.to_string(),
            GlobalName::Function {
                id,
                parameter_count: parameters_count as u32,
            },
        );
        Ok(Function {
            parameters_count,
            instructions,
        })
    }
}

pub fn compile_program(text: &str) -> Result<Program, PestError> {
    let mut begin_instructions = Vec::new();
    let mut rules = Vec::new();
    let mut end_instructions = Vec::new();
    let mut functions = Vec::new();

    let mut compiler = Compiler::default();
    let program = AwkParser::parse(Rule::program, text)?.next().unwrap();

    for item in program.into_inner() {
        match item.as_rule() {
            Rule::begin_action => {
                compiler.compile_action(
                    first_child(item),
                    &mut begin_instructions,
                    &HashMap::new(),
                )?;
            }
            Rule::end_action => {
                compiler.compile_action(
                    first_child(item),
                    &mut end_instructions,
                    &HashMap::new(),
                )?;
            }
            Rule::rule => {
                rules.push(compiler.compile_rule(item)?);
            }
            Rule::function_definition => {
                functions.push(compiler.compile_function_definition(item)?);
            }
            Rule::EOI => {}
            _ => unreachable!("encontered {:?} while compiling program", item.as_rule()),
        }
    }

    Ok(Program {
        constants: compiler.constants.into_inner(),
        begin_instructions,
        rules,
        end_instructions,
        functions,
        globals_count: compiler.last_global_var_id.get() as usize,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::regex::regex_from_str;

    const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

    fn compile_expr(expr: &str) -> (Vec<OpCode>, Vec<Constant>) {
        let mut program = compile_program(format!("BEGIN {{ {} }}", expr).as_str())
            .expect("error compiling expression");
        // remove OpCode::Pop
        program.begin_instructions.pop();
        (program.begin_instructions, program.constants)
    }

    fn compile_stmt(stmt: &str) -> (Vec<OpCode>, Vec<Constant>) {
        let mut program = compile_program(format!("BEGIN {{ {} }}", stmt).as_str())
            .expect("error compiling statement");
        (program.begin_instructions, program.constants)
    }

    fn compile_correct_program(text: &str) -> Program {
        compile_program(text).expect("error compiling program")
    }

    fn does_not_compile(text: &str) {
        compile_program(text).expect_err("expected error compiling program");
    }

    #[test]
    fn test_compile_empty_program() {
        let program = compile_correct_program("");
        assert!(program.constants.is_empty());
        assert!(program.begin_instructions.is_empty());
        assert!(program.rules.is_empty());
        assert!(program.end_instructions.is_empty());
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_compile_empty_begin() {
        let program = compile_correct_program("BEGIN {}");
        assert!(program.constants.is_empty());
        assert!(program.begin_instructions.is_empty());
        assert!(program.rules.is_empty());
        assert!(program.end_instructions.is_empty());
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_compile_empty_end() {
        let program = compile_correct_program("END {}");
        assert!(program.constants.is_empty());
        assert!(program.begin_instructions.is_empty());
        assert!(program.rules.is_empty());
        assert!(program.end_instructions.is_empty());
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_compile_numbers() {
        let (_, constants) = compile_expr("123");
        assert_eq!(constants, vec![Constant::Number(123.0)]);

        let (_, constants) = compile_expr("1.23");
        assert_eq!(constants, vec![Constant::Number(1.23)]);

        let (_, constants) = compile_expr(".154");
        assert_eq!(constants, vec![Constant::Number(0.154)]);

        let (_, constants) = compile_expr("1.");
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (_, constants) = compile_expr("1.0e3");
        assert_eq!(constants, vec![Constant::Number(1.0e3)]);

        let (_, constants) = compile_expr("1.0e-3");
        assert_eq!(constants, vec![Constant::Number(1.0e-3)]);

        let (_, constants) = compile_expr("1.0e+3");
        assert_eq!(constants, vec![Constant::Number(1.0e+3)]);

        let (_, constants) = compile_expr("1e5");
        assert_eq!(constants, vec![Constant::Number(1e5)]);

        let (_, constants) = compile_expr("5.f");
        assert_eq!(constants, vec![Constant::Number(5.0)]);

        let (_, constants) = compile_expr("5.34F");
        assert_eq!(constants, vec![Constant::Number(5.34)]);

        let (_, constants) = compile_expr("5.34l");
        assert_eq!(constants, vec![Constant::Number(5.34)]);

        let (_, constants) = compile_expr("5.34L");
        assert_eq!(constants, vec![Constant::Number(5.34)]);
    }

    #[test]
    fn test_compile_string() {
        let (_, constants) = compile_expr(r#""hello""#);
        assert_eq!(constants, vec![Constant::String("hello".to_string())]);

        let (_, constants) = compile_expr(r#""hello\nworld""#);
        assert_eq!(
            constants,
            vec![Constant::String("hello\nworld".to_string())]
        );

        let (_, constants) = compile_expr(r#""hello\tworld""#);
        assert_eq!(
            constants,
            vec![Constant::String("hello\tworld".to_string())]
        );

        let (_, constants) = compile_expr(r#""hello\\world""#);
        assert_eq!(
            constants,
            vec![Constant::String("hello\\world".to_string())]
        );

        let (_, constants) = compile_expr(r#""hello\"world""#);
        assert_eq!(
            constants,
            vec![Constant::String(r#"hello"world"#.to_string())]
        );

        let (_, constants) = compile_expr(r#""hello\0world""#);
        assert_eq!(
            constants,
            vec![Constant::String("hello\x00world".to_string())]
        );

        let (_, constants) = compile_expr(r#""hello\41world""#);
        assert_eq!(constants, vec![Constant::String("hello!world".to_string())]);

        let (_, constants) = compile_expr(r#""hello\141world""#);
        assert_eq!(constants, vec![Constant::String("helloaworld".to_string())]);
    }

    #[test]
    fn test_compile_unary_numeric_ops() {
        let (instructions, _) = compile_expr("-1");
        assert_eq!(instructions, vec![OpCode::PushConstant(0), OpCode::Negate]);

        let (instructions, _) = compile_expr("+1");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::AsNumber]
        );

        let (instructions, _) = compile_expr("!1");
        assert_eq!(instructions, vec![OpCode::PushConstant(0), OpCode::Not]);

        let (instructions, _) = compile_expr("++a");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PreInc]
        );

        let (instructions, _) = compile_expr("--a");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PreDec]
        );

        let (instructions, _) = compile_expr("a++");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PostInc]
        );

        let (instructions, _) = compile_expr("a--");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PostDec]
        );

        let (instructions, _) = compile_expr("++a[0]");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::IndexArray,
                OpCode::PreInc
            ]
        );

        let (instructions, _) = compile_expr("++$3");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::FieldRef, OpCode::PreInc]
        );
    }

    #[test]
    fn test_compile_binary_numeric_ops() {
        let (instructions, _) = compile_expr("1 + 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Add,
            ]
        );

        let (instructions, _) = compile_expr("1 - 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Sub,
            ]
        );

        let (instructions, _) = compile_expr("1 * 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Mul,
            ]
        );

        let (instructions, _) = compile_expr("1 / 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Div,
            ]
        );

        let (instructions, _) = compile_expr("1 % 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Mod,
            ]
        );

        let (instructions, _) = compile_expr("1 ^ 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Pow,
            ]
        );
    }

    #[test]
    fn test_exp_is_right_associative() {
        let (instructions, _) = compile_expr("1 ^ 2 ^ 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Pow,
                OpCode::Pow,
            ]
        );
    }

    #[test]
    fn test_compile_binary_numeric_exprs_with_correct_precedence() {
        let (instructions, constants) = compile_expr("1 + 2 * 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Mul,
                OpCode::Add,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::Number(3.0),
            ]
        );

        let (instructions, constants) = compile_expr("3 * 8 + 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Mul,
                OpCode::PushConstant(2),
                OpCode::Add,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(3.0),
                Constant::Number(8.0),
                Constant::Number(1.0),
            ]
        );

        let (instructions, constants) = compile_expr("34 + 7 / 3.45");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Div,
                OpCode::Add,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(34.0),
                Constant::Number(7.0),
                Constant::Number(3.45),
            ]
        );

        let (instructions, _) = compile_expr("1 / 2 + 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Div,
                OpCode::PushConstant(2),
                OpCode::Add,
            ]
        );

        let (instructions, _) = compile_expr("1 + 2 % 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Mod,
                OpCode::Add,
            ]
        );

        let (instructions, _) = compile_expr("1 % 2 + 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Mod,
                OpCode::PushConstant(2),
                OpCode::Add,
            ]
        );

        let (instructions, _) = compile_expr("1 + 2 ^ 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Pow,
                OpCode::Add,
            ]
        );

        let (instructions, _) = compile_expr("1 ^ 2 * 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Pow,
                OpCode::PushConstant(2),
                OpCode::Mul,
            ]
        );
    }

    #[test]
    fn compile_concat() {
        let (instructions, constants) = compile_expr(r#""hello" "world""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Concat,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::String("hello".to_string()),
                Constant::String("world".to_string()),
            ]
        );

        let (instructions, constants) = compile_expr(r#""hello" 1 "world""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Concat,
                OpCode::PushConstant(2),
                OpCode::Concat,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::String("hello".to_string()),
                Constant::Number(1.0),
                Constant::String("world".to_string()),
            ]
        );

        let (instructions, constants) = compile_expr(r#""hello"1"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Concat,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::String("hello".to_string()), Constant::Number(1.0)]
        );

        let (instructions, constants) = compile_expr(r#"1"hello""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Concat,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::String("hello".to_string())]
        );
    }

    #[test]
    fn test_compile_comp_op() {
        let (instructions, constants) = compile_expr(r#"1 < "x""#);
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Lt]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::String("x".to_string())]
        );

        let (instructions, constants) = compile_expr("1 > 2");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Gt]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr("1 <= 2");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr(r#" "str" >= 2"#);
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge]
        );
        assert_eq!(
            constants,
            vec![Constant::String("str".to_string()), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr("1 == 2");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Eq]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr("1 != 2");
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ne]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );
    }

    #[test]
    fn test_compile_in_expr() {
        let (instructions, constants) = compile_expr(r#""a" in map"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::In
            ]
        );
        assert_eq!(constants, vec![Constant::String("a".to_string())]);
    }

    #[test]
    fn test_compile_match() {
        let (instructions, constants) = compile_expr(r#" "hello" ~ /hello/ "#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Match
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::String("hello".to_string()),
                Constant::Regex(Rc::new(regex_from_str("hello")))
            ]
        )
    }

    #[test]
    fn test_compile_not_match() {
        let (instructions, constants) = compile_expr(r#" "test" !~ /te?s+t*/"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Match,
                OpCode::Not
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::String("test".to_string()),
                Constant::Regex(Rc::new(regex_from_str("te?s+t*")))
            ]
        )
    }

    #[test]
    fn test_compile_and() {
        let (instructions, constants) = compile_expr("1 && 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(2),
                OpCode::PushConstant(1),
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr(r#"1 < 2 && "x" >= "y""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfFalse(4),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::Ge,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::String("x".to_string()),
                Constant::String("y".to_string()),
            ]
        );
    }

    #[test]
    fn test_compile_or() {
        let (instructions, constants) = compile_expr("1 || 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfTrue(3),
                OpCode::PushConstant(1),
                OpCode::Jump(1),
                OpCode::PushOne,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr(r#"1 < 2 || "x" >= "y""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfTrue(5),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::Ge,
                OpCode::Jump(1),
                OpCode::PushOne,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::String("x".to_string()),
                Constant::String("y".to_string()),
            ]
        );
    }

    #[test]
    fn test_compile_ternary_expression() {
        let (instructions, constants) = compile_expr("1 ? 2 : 3");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Jump(2),
                OpCode::PushConstant(2),
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::Number(3.0),
            ]
        );

        let (instructions, constants) = compile_expr(r#"a == 1 ? "one" : a == 2 ? "two" : "many""#);
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Eq,
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Jump(8),
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(2),
                OpCode::Eq,
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(3),
                OpCode::Jump(2),
                OpCode::PushConstant(4),
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::String("one".to_string()),
                Constant::Number(2.0),
                Constant::String("two".to_string()),
                Constant::String("many".to_string()),
            ]
        );
    }

    #[test]
    fn test_compile_simple_variable_assignment() {
        let (instructions, constants) = compile_expr("a = 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a = b = 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::GlobalRef(FIRST_GLOBAL_VAR + 1),
                OpCode::PushConstant(0),
                OpCode::Assign,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(2.0)]);
    }

    #[test]
    fn test_compile_compound_variable_assignment() {
        let (instructions, constants) = compile_expr("a += 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Add,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a -= 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Sub,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a *= 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Mul,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a /= 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Div,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a %= 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Mod,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a ^= 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Pow,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a += b += 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::GlobalRef(FIRST_GLOBAL_VAR + 1),
                OpCode::Dup,
                OpCode::PushConstant(0),
                OpCode::Add,
                OpCode::Assign,
                OpCode::Add,
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);
    }

    #[test]
    fn compile_array_element_assignment() {
        let (instructions, constants) = compile_expr("a[1] = 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::IndexArray,
                OpCode::PushConstant(1),
                OpCode::Assign,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(1.0)]
        );
    }

    #[test]
    fn compile_filed_var_assignment() {
        let (instructions, constants) = compile_expr("$1 = 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::FieldRef,
                OpCode::PushConstant(1),
                OpCode::Assign,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(1.0)]
        );
    }

    #[test]
    fn compile_terminated_if() {
        let (instructions, constant) = compile_stmt("if (1) \n 1;");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Pop,
            ]
        );
        assert_eq!(constant, vec![Constant::Number(1.0), Constant::Number(1.0)]);

        let (instructions, constant) = compile_stmt("if (1) \n 1; \n else \n 2;");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(4),
                OpCode::PushConstant(1),
                OpCode::Pop,
                OpCode::Jump(3),
                OpCode::PushConstant(2),
                OpCode::Pop,
            ]
        );
        assert_eq!(
            constant,
            vec![
                Constant::Number(1.0),
                Constant::Number(1.0),
                Constant::Number(2.0)
            ]
        );
    }

    #[test]
    fn compile_terminated_while() {
        let (instructions, constant) = compile_stmt("while (1) \n 1;");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(4),
                OpCode::PushConstant(1),
                OpCode::Pop,
                OpCode::Jump(-4),
            ]
        );
        assert_eq!(constant, vec![Constant::Number(1.0), Constant::Number(1.0)]);

        let (instructions, constant) = compile_stmt("while (1) {1; 2; 3;}");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(8),
                OpCode::PushConstant(1),
                OpCode::Pop,
                OpCode::PushConstant(2),
                OpCode::Pop,
                OpCode::PushConstant(3),
                OpCode::Pop,
                OpCode::Jump(-8),
            ]
        );
        assert_eq!(
            constant,
            vec![
                Constant::Number(1.0),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_terminated_for() {
        let (instructions, constant) = compile_stmt("for (i = 0; i < 10; i++) 1;");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
                OpCode::Pop,
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfFalse(7),
                OpCode::PushConstant(2),
                OpCode::Pop,
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PostInc,
                OpCode::Pop,
                OpCode::Jump(-9),
            ]
        );
        assert_eq!(
            constant,
            vec![
                Constant::Number(0.0),
                Constant::Number(10.0),
                Constant::Number(1.0),
            ]
        );
    }

    #[test]
    fn test_compile_next() {
        let (instructions, _) = compile_stmt("next;");
        assert_eq!(instructions, vec![OpCode::Next]);
    }

    #[test]
    fn test_compile_exit() {
        let (instructions, _) = compile_stmt("exit;");
        assert_eq!(instructions, vec![OpCode::Exit]);

        let (instructions, constant) = compile_stmt("exit 1;");
        assert_eq!(instructions, vec![OpCode::PushConstant(0), OpCode::Exit]);
    }

    #[test]
    fn test_compile_do_while() {
        let (instructions, constant) = compile_stmt("do 1; while (1);");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::Pop,
                OpCode::PushConstant(1),
                OpCode::JumpIfTrue(-3),
            ]
        );
        assert_eq!(constant, vec![Constant::Number(1.0), Constant::Number(1.0)]);
    }

    #[test]
    fn test_compile_delete() {
        let (instructions, constant) = compile_stmt("delete a[1];");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Delete,
            ]
        );
    }

    #[test]
    fn test_compile_simple_print() {
        let (instructions, constant) = compile_stmt("print 1;");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 1
                },
            ]
        );
        assert_eq!(constant, vec![Constant::Number(1.0),]);

        let (instructions, constant) = compile_stmt(r#"print "number", 1, 2, "and", 3;"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 5
                },
            ]
        );
        assert_eq!(
            constant,
            vec![
                Constant::String("number".to_string()),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::String("and".to_string()),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_print_call() {
        let (instructions, constant) = compile_stmt("print (\"hello\");");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 1
                },
            ]
        );
        assert_eq!(constant, vec![Constant::String("hello".to_string())]);

        let (instructions, constants) = compile_stmt(r#"print ("hello", 1, 2, "and", 3);"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 5
                },
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::String("hello".to_string()),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::String("and".to_string()),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_empty_function() {
        let program = compile_correct_program(
            r#"
            function fun() {}
            "#,
        );
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].parameters_count, 0);
    }

    #[test]
    fn test_compile_function_with_no_parameters() {
        let program = compile_correct_program(
            r#"
            function fun() {
                x + 2;
            }
            "#,
        );
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].parameters_count, 0);
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::GlobalRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Add,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
    }

    #[test]
    fn test_compile_function_with_parameters() {
        let program = compile_correct_program(
            r#"
            function fun(a, b, c) {
                a["1"] = b + c;
            }
            "#,
        );
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].parameters_count, 3);
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::LocalRef(0),
                OpCode::PushConstant(0),
                OpCode::IndexArray,
                OpCode::LocalRef(1),
                OpCode::LocalRef(2),
                OpCode::Add,
                OpCode::Assign,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
    }

    #[test]
    fn test_compile_function_call_no_params() {
        let program = compile_correct_program(
            r#"
            function fun() {
            }
            BEGIN {fun()}
            "#,
        );
        assert_eq!(
            program.begin_instructions,
            vec![OpCode::Call { id: 0, argc: 0 }, OpCode::Pop]
        );
    }

    #[test]
    fn test_compile_function_call() {
        let program = compile_correct_program(
            r#"
            function fun(a, b) {
                a + b;
            }
            BEGIN {fun(1, 2)}
            "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::LocalRef(0),
                OpCode::LocalRef(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
        assert_eq!(
            program.begin_instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Call { id: 0, argc: 2 },
                OpCode::Pop,
            ]
        );
    }

    #[test]
    fn test_compile_function_call_with_too_few_arguments() {
        let program = compile_correct_program(
            r#"
            function fun(a, b) {
                a + b;
            }
            BEGIN {fun(1)}
            "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::LocalRef(0),
                OpCode::LocalRef(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
        assert_eq!(
            program.begin_instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushUninitialized,
                OpCode::Call { id: 0, argc: 2 },
                OpCode::Pop,
            ]
        );
    }

    #[test]
    fn test_compile_empty_return_statement() {
        let program = compile_correct_program(
            r#"
            function fun() {
                return;
            }
            "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![OpCode::PushUninitializedScalar, OpCode::Return]
        );
    }

    #[test]
    fn test_compile_return_statement_with_expression() {
        let program = compile_correct_program(
            r#"
            function fun() {
                return 1;
            }
            "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![OpCode::PushConstant(0), OpCode::Return]
        );
    }

    #[test]
    fn test_return_statement_outside_of_function_is_err() {
        does_not_compile("BEGIN { return 1; }");
    }

    #[test]
    fn compile_rule_with_expression_pattern() {
        let program = compile_correct_program(
            r#"
            1 {
                1 + 2;
            }
            "#,
        );
        assert_eq!(program.rules.len(), 1);
        assert_eq!(
            program.rules[0].pattern,
            Pattern::Expr(vec![OpCode::PushConstant(0)])
        );
        assert_eq!(
            program.rules[0].instructions,
            vec![
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::Add,
                OpCode::Pop,
            ]
        );
        assert_eq!(
            program.constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(1.0),
                Constant::Number(2.0)
            ]
        );
    }

    #[test]
    fn compile_rule_with_range_pattern() {
        let program = compile_correct_program(
            r#"
            1, 2 {
                1 + 2;
            }
            "#,
        );
        assert_eq!(program.rules.len(), 1);
        assert_eq!(
            program.rules[0].pattern,
            Pattern::Range {
                start: vec![OpCode::PushConstant(0)],
                end: vec![OpCode::PushConstant(1)]
            }
        );
        assert_eq!(
            program.rules[0].instructions,
            vec![
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::Add,
                OpCode::Pop,
            ]
        );
        assert_eq!(
            program.constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::Number(1.0),
                Constant::Number(2.0)
            ]
        );
    }

    #[test]
    fn compile_rule_without_pattern() {
        let program = compile_correct_program(
            r#"
            {1}
            "#,
        );
        assert_eq!(program.rules.len(), 1);
        assert_eq!(program.rules[0].pattern, Pattern::All);
        assert_eq!(
            program.rules[0].instructions,
            vec![OpCode::PushConstant(0), OpCode::Pop,]
        );
        assert_eq!(program.constants, vec![Constant::Number(1.0)]);
    }

    #[test]
    fn compile_builtin_arithmetic_functions() {
        let program = compile_correct_program(
            r#"
            BEGIN {
                atan2(1, 2);
                cos(1);
                sin(1);
                exp(1);
                log(1);
                sqrt(1);
                int(1);
                rand();
                srand();
                srand(1);
            }
            "#,
        );
        assert_eq!(
            program.begin_instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Atan2,
                    argc: 2
                },
                OpCode::Pop,
                OpCode::PushConstant(2),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Cos,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::PushConstant(3),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sin,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Exp,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::PushConstant(5),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Log,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::PushConstant(6),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sqrt,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::PushConstant(7),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Int,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Rand,
                    argc: 0
                },
                OpCode::Pop,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Srand,
                    argc: 0
                },
                OpCode::Pop,
                OpCode::PushConstant(8),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Srand,
                    argc: 1
                },
                OpCode::Pop,
            ]
        );
    }

    #[test]
    fn test_compile_bultin_string_functions() {
        let program = compile_correct_program(
            r#"
            BEGIN {
                gsub("a", "b");
                gsub("a", "b", "c");
                index("a", "b");
                match("a", "b");
                split("a", "b");
                split("a", "b", "c");
                length;
                length();
                length("a");
                sprintf("a", 1);
                sprintf("a", 1, 2, 3, 4, 5);
                sub("a", "b");
                sub("a", "b", "c");
                substr("a", 1);
                substr("a", 1, 2);
                tolower("A");
                toupper("a");
            }
        "#,
        );
        assert_eq!(
            program.begin_instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Gsub,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Gsub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(5),
                OpCode::PushConstant(6),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Index,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(7),
                OpCode::PushConstant(8),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Match,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(9),
                OpCode::PushConstant(10),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Split,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(11),
                OpCode::PushConstant(12),
                OpCode::PushConstant(13),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Split,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 0,
                },
                OpCode::Pop,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 0,
                },
                OpCode::Pop,
                OpCode::PushConstant(14),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(15),
                OpCode::PushConstant(16),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sprintf,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(17),
                OpCode::PushConstant(18),
                OpCode::PushConstant(19),
                OpCode::PushConstant(20),
                OpCode::PushConstant(21),
                OpCode::PushConstant(22),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sprintf,
                    argc: 6,
                },
                OpCode::Pop,
                OpCode::PushConstant(23),
                OpCode::PushConstant(24),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sub,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(25),
                OpCode::PushConstant(26),
                OpCode::PushConstant(27),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(28),
                OpCode::PushConstant(29),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Substr,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(30),
                OpCode::PushConstant(31),
                OpCode::PushConstant(32),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Substr,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(33),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::ToLower,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(34),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::ToUpper,
                    argc: 1,
                },
                OpCode::Pop,
            ],
        );
    }

    #[test]
    fn test_compile_builtin_io_functions() {
        let program = compile_correct_program(
            r#"
            BEGIN {
                close("file");
                system("ls");
            }
        "#,
        );
        assert_eq!(
            program.begin_instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Close,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::System,
                    argc: 1,
                },
                OpCode::Pop,
            ]
        );
    }
}
