//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#![allow(clippy::result_large_err)]

use crate::program::{
    Action, AwkRule, BuiltinFunction, Constant, DebugInfo, Function, OpCode, Pattern, Program,
    SourceLocation, SpecialVar, VarId,
};
use crate::regex::Regex;
use pest::error::InputLocation;
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
        (Rule::fflush, BuiltinFunctionInfo {
            function: BuiltinFunction::FFlush,
            min_args: 0,
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
            if char_code == 0 {
                return Err("invalid escape sequence: \\0".to_string());
            }
            char::from_u32(char_code).ok_or("invalid character")?
        }
        other => return Err(format!("invalid escape sequence: \\{}", other)),
    };
    let char_after_escape_sequence = char_after_escape_sequence.or_else(|| iter.next());
    Ok((escaped_char, char_after_escape_sequence))
}

pub fn escape_string_contents(s: &str) -> Result<Rc<str>, String> {
    let mut result = String::new();
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
    Ok(result.into())
}

fn post_increment(val: &Cell<u32>) -> u32 {
    let result = val.get();
    val.set(result + 1);
    result
}

fn parse_float(val: &str) -> f64 {
    lexical::parse_partial_with_options::<f64, _, { lexical::format::C_LITERAL }>(
        val,
        &lexical::ParseFloatOptions::default(),
    )
    .unwrap()
    .0
}

fn lvalue_to_scalar_ref(instructions: &mut [OpCode]) {
    let last_ref = instructions
        .last_mut()
        .expect("lvalue expression has no instructions");
    match *last_ref {
        OpCode::GetGlobal(ref id) => *last_ref = OpCode::GlobalScalarRef(*id),
        OpCode::GetLocal(ref id) => *last_ref = OpCode::LocalScalarRef(*id),
        OpCode::GetField => *last_ref = OpCode::FieldRef,
        OpCode::IndexArrayGetValue => *last_ref = OpCode::IndexArrayGetRef,
        _ => unreachable!(),
    }
}

fn normalize_builtin_function_arguments(
    function: BuiltinFunction,
    mut args: Vec<Instructions>,
    line_col: (usize, usize),
) -> (Instructions, u16) {
    let flatten = |args: Vec<Instructions>| {
        args.into_iter()
            .fold(Instructions::default(), |mut acc, i| {
                acc.extend(i);
                acc
            })
    };
    let argc = args.len() as u16;
    match function {
        BuiltinFunction::Length => {
            if args.is_empty() {
                (
                    Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushZero, OpCode::GetField],
                        line_col,
                    ),
                    1,
                )
            } else {
                (args.into_iter().next().unwrap(), 1)
            }
        }
        BuiltinFunction::Split => {
            // put the array as the first argument
            args[0..2].rotate_right(1);
            (flatten(args), argc)
        }
        BuiltinFunction::Sub | BuiltinFunction::Gsub => {
            if argc == 2 {
                let mut instructions = Instructions::from_instructions_and_line_col(
                    vec![OpCode::PushZero, OpCode::FieldRef],
                    line_col,
                );
                instructions.extend(flatten(args));
                (instructions, 3)
            } else {
                args.rotate_right(1);
                lvalue_to_scalar_ref(&mut args[0].opcodes);
                (flatten(args), 3)
            }
        }
        _ => (flatten(args), argc),
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq)]
enum ExprKind {
    LValue,
    Number,
    String,
    Regex,
    Comp,
}

struct Expr {
    kind: ExprKind,
    instructions: Instructions,
}

impl Expr {
    fn new(kind: ExprKind, instructions: Instructions) -> Self {
        Expr { kind, instructions }
    }
}

#[derive(Clone, Copy)]
pub enum GlobalName {
    Variable(VarId),
    SpecialVar(VarId),
    Function { id: u32, parameter_count: u32 },
}

type NameMap = HashMap<String, GlobalName>;
type LocalMap = HashMap<String, VarId>;

#[cfg_attr(test, derive(Debug))]
#[derive(Default)]
struct Instructions {
    opcodes: Vec<OpCode>,
    source_locations: Vec<SourceLocation>,
}

impl Instructions {
    fn from_instructions_and_line_col(instructions: Vec<OpCode>, line_col: (usize, usize)) -> Self {
        let source_locations = vec![
            SourceLocation {
                line: line_col.0 as u32,
                column: line_col.1 as u32,
            };
            instructions.len()
        ];
        Instructions {
            opcodes: instructions,
            source_locations,
        }
    }

    fn push(&mut self, instruction: OpCode, line_col: (usize, usize)) {
        self.opcodes.push(instruction);
        self.source_locations.push(SourceLocation {
            line: line_col.0 as u32,
            column: line_col.1 as u32,
        });
    }

    fn extend(&mut self, instructions: Instructions) {
        self.opcodes.extend(instructions.opcodes);
        self.source_locations.extend(instructions.source_locations);
    }

    fn into_action(self, file: Rc<str>) -> Action {
        Action {
            instructions: self.opcodes,
            debug_info: DebugInfo {
                source_locations: self.source_locations,
                file,
            },
        }
    }

    fn len(&self) -> usize {
        assert_eq!(self.opcodes.len(), self.source_locations.len());
        self.opcodes.len()
    }
}

#[derive(Default)]
struct LoopStubs {
    break_stubs: Vec<usize>,
    continue_stubs: Vec<usize>,
}

struct Compiler {
    constants: RefCell<Vec<Constant>>,
    names: RefCell<NameMap>,
    last_global_var_id: Cell<u32>,
    last_global_function_id: Cell<u32>,
    in_function: bool,
    loop_stack: Vec<LoopStubs>,
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
                "FS".to_string(),
                GlobalName::SpecialVar(SpecialVar::Fs as u32),
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
                "RLENGTH".to_string(),
                GlobalName::SpecialVar(SpecialVar::Rlength as u32),
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
            loop_stack: Vec::new(),
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
        if let Some(local_id) = locals.get(name) {
            Ok(OpCode::GetLocal(*local_id))
        } else {
            let entry = self.names.borrow().get(name).copied();
            if let Some(var) = entry {
                match var {
                    GlobalName::Variable(id) => Ok(OpCode::GetGlobal(id)),
                    GlobalName::SpecialVar(id) => Ok(OpCode::GetGlobal(id)),
                    GlobalName::Function { .. } => {
                        Err(format!("'{}' function used in variable context", name))
                    }
                }
            } else {
                let id = post_increment(&self.last_global_var_id);
                self.names
                    .borrow_mut()
                    .insert(name.to_string(), GlobalName::Variable(id));
                Ok(OpCode::GetGlobal(id))
            }
        }
    }

    fn compile_function_args(
        &self,
        args: Pairs<Rule>,
        instructions: &mut Instructions,
        call_span: pest::Span,
        locals: &LocalMap,
    ) -> Result<u16, PestError> {
        let mut argc: u32 = 0;
        for arg in args {
            self.compile_expr(arg, instructions, locals)?;
            argc += 1;
            if argc > u16::MAX as u32 {
                return Err(pest_error_from_span(
                    call_span,
                    "function call with too many arguments".to_string(),
                ));
            }
        }
        Ok(argc as u16)
    }

    fn map_primary(&self, primary: Pair<Rule>, locals: &LocalMap) -> Result<Expr, PestError> {
        match primary.as_rule() {
            Rule::expr => {
                let mut instructions = Instructions::default();
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
                    Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushConstant(index)],
                        primary.line_col(),
                    ),
                ))
            }
            Rule::number => {
                let num = parse_float(primary.as_str());
                let index = self.push_constant(Constant::Number(num));
                Ok(Expr::new(
                    ExprKind::Number,
                    Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushConstant(index)],
                        primary.line_col(),
                    ),
                ))
            }
            Rule::string => {
                let span = primary.as_span();
                let string_line_col = primary.line_col();
                let str = escape_string_contents(first_child(primary).as_str())
                    .map_err(|e| pest_error_from_span(span, e))?;
                let index = self.push_constant(Constant::String(str));
                Ok(Expr::new(
                    ExprKind::String,
                    Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushConstant(index)],
                        string_line_col,
                    ),
                ))
            }
            Rule::lvalue => {
                let mut instructions = Instructions::default();
                self.compile_lvalue(primary, &mut instructions, locals)?;
                Ok(Expr::new(ExprKind::LValue, instructions))
            }
            Rule::function_call => {
                let span = primary.as_span();
                let line_col = primary.line_col();
                let mut inner = primary.into_inner();
                let name = inner.next().unwrap().as_str();
                let mut instructions = Instructions::default();
                let argc = self.compile_function_args(inner, &mut instructions, span, locals)?;
                match self.names.borrow().get(name) {
                    Some(GlobalName::Function {
                        id,
                        parameter_count,
                    }) => {
                        // I think this is a better way to structure the code
                        #[allow(clippy::comparison_chain)]
                        if argc > *parameter_count as u16 {
                            // other implementations issue a warning here
                            // but I think it's better to error out
                            return Err(pest_error_from_span(
                                span,
                                format!("function '{}' called with too many arguments", name),
                            ));
                        } else if argc < *parameter_count as u16 {
                            for _ in argc..*parameter_count as u16 {
                                instructions.push(OpCode::PushUninitialized, line_col);
                            }
                        }
                        instructions.push(OpCode::Call(*id), line_col);
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
                let line_col = primary.line_col();
                let mut inner = primary.into_inner();
                let function = inner.next().unwrap();
                let mut args = Vec::new();
                for arg in inner {
                    let mut arg_instructions = Instructions::default();
                    self.compile_expr(arg, &mut arg_instructions, locals)?;
                    args.push(arg_instructions);
                    if args.len() > u16::MAX as usize {
                        return Err(pest_error_from_span(
                            span,
                            "function call with too many arguments".to_string(),
                        ));
                    }
                }
                let argc = args.len() as u16;
                let fn_info = BUILTIN_FUNCTIONS
                    .get(&function.as_rule())
                    .expect("missing builtin");
                if (fn_info.min_args..=fn_info.max_args).contains(&argc) {
                    let (mut instructions, argc) =
                        normalize_builtin_function_arguments(fn_info.function, args, line_col);
                    instructions.push(
                        OpCode::CallBuiltin {
                            function: fn_info.function,
                            argc,
                        },
                        line_col,
                    );
                    Ok(Expr::new(ExprKind::Number, instructions))
                } else {
                    Err(pest_error_from_span(
                        span,
                        format!(
                            "incorrect number of arguments for builtin function '{}'",
                            function.as_str()
                        ),
                    ))
                }
            }
            _ => unreachable!(
                "encountered {:?} while compiling primary",
                primary.as_rule()
            ),
        }
    }

    fn map_prefix(&self, op: Pair<Rule>, rhs: Expr) -> Result<Expr, PestError> {
        let kind = rhs.kind;
        let mut instructions = rhs.instructions;
        match op.as_rule() {
            Rule::negate => {
                instructions.push(OpCode::Negate, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::not => {
                instructions.push(OpCode::Not, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::unary_plus => {
                instructions.push(OpCode::AsNumber, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::pre_inc | Rule::pre_dec => {
                if kind != ExprKind::LValue {
                    return Err(pest_error_from_span(
                        op.as_span(),
                        "operand should be an lvalue".to_string(),
                    ));
                }
                lvalue_to_scalar_ref(&mut instructions.opcodes);
                if op.as_rule() == Rule::pre_inc {
                    instructions.push(OpCode::PreInc, op.line_col());
                } else {
                    instructions.push(OpCode::PreDec, op.line_col());
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
        lvalue_to_scalar_ref(&mut instructions.opcodes);
        if op.as_rule() == Rule::post_inc {
            instructions.push(OpCode::PostInc, op.line_col());
        } else {
            instructions.push(OpCode::PostDec, op.line_col());
        }
        Ok(Expr::new(ExprKind::Number, instructions))
    }

    fn map_infix(&self, lhs: Expr, op: Pair<Rule>, rhs: Expr) -> Result<Expr, PestError> {
        let lhs_kind = lhs.kind;
        let rhs_kind = rhs.kind;
        let mut instructions = lhs.instructions;

        match op.as_rule() {
            Rule::and => {
                instructions.push(
                    OpCode::JumpIfFalse(rhs.instructions.len() as i32 + 2),
                    op.line_col(),
                );
                instructions.extend(rhs.instructions);
                instructions.push(OpCode::Jump(2), op.line_col());
                instructions.push(OpCode::PushZero, op.line_col());
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            Rule::or => {
                instructions.push(
                    OpCode::JumpIfTrue(rhs.instructions.len() as i32 + 2),
                    op.line_col(),
                );
                instructions.extend(rhs.instructions);
                instructions.push(OpCode::Jump(2), op.line_col());
                instructions.push(OpCode::PushOne, op.line_col());
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            Rule::in_op => {
                let lhs_instructions = instructions;
                let mut instructions = rhs.instructions;
                lvalue_to_scalar_ref(&mut instructions.opcodes);
                instructions.extend(lhs_instructions);
                instructions.push(OpCode::In, op.line_col());
                return Ok(Expr::new(ExprKind::Number, instructions));
            }
            _ => {}
        }

        instructions.extend(rhs.instructions);
        match op.as_rule() {
            Rule::add => {
                instructions.push(OpCode::Add, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::binary_sub => {
                instructions.push(OpCode::Sub, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::mul => {
                instructions.push(OpCode::Mul, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::div => {
                instructions.push(OpCode::Div, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::modulus => {
                instructions.push(OpCode::Mod, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::pow => {
                instructions.push(OpCode::Pow, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::le => {
                instructions.push(OpCode::Le, op.line_col());
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
                instructions.push(
                    match op.as_rule() {
                        Rule::lt => OpCode::Lt,
                        Rule::gt => OpCode::Gt,
                        Rule::le => OpCode::Le,
                        Rule::ge => OpCode::Ge,
                        Rule::eq => OpCode::Eq,
                        Rule::ne => OpCode::Ne,
                        _ => unreachable!(),
                    },
                    op.line_col(),
                );
                Ok(Expr::new(ExprKind::Comp, instructions))
            }
            Rule::match_op => {
                instructions.push(OpCode::Match, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::not_match => {
                instructions.push(OpCode::Match, op.line_col());
                instructions.push(OpCode::Not, op.line_col());
                Ok(Expr::new(ExprKind::Number, instructions))
            }
            Rule::concat => {
                instructions.push(OpCode::Concat, op.line_col());
                Ok(Expr::new(ExprKind::String, instructions))
            }
            _ => unreachable!(),
        }
    }

    fn compile_simple_binary_expr(
        &self,
        expr: Pairs<Rule>,
        locals: &LocalMap,
    ) -> Result<Expr, PestError> {
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
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let lvalue = first_child(lvalue);
        let line_col = lvalue.line_col();
        match lvalue.as_rule() {
            Rule::name => {
                let get_instruction = self
                    .get_var(lvalue.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(lvalue.as_span(), msg))?;
                instructions.push(get_instruction, line_col);
            }
            Rule::array_element => {
                let mut inner = lvalue.into_inner();
                let name = inner.next().unwrap();
                let get_instruction = self
                    .get_var(name.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(name.as_span(), msg))?;
                instructions.push(get_instruction, line_col);
                self.compile_array_index(inner, instructions, locals)?;
                instructions.push(OpCode::IndexArrayGetValue, line_col)
            }
            Rule::field_var => {
                let primary = self.map_primary(first_child(lvalue), locals)?;
                instructions.extend(primary.instructions);
                instructions.push(OpCode::GetField, line_col);
            }
            _ => unreachable!("encountered {:?} while compiling lvalue", lvalue.as_rule()),
        }
        Ok(())
    }

    fn compile_array_index(
        &self,
        mut index: Pairs<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let first_index = index.next().unwrap();
        self.compile_expr(first_index, instructions, locals)?;
        if let Some(second_index) = index.next() {
            let second_index_line_col = second_index.line_col();
            instructions.push(
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                second_index.line_col(),
            );
            instructions.push(OpCode::Concat, second_index.line_col());
            self.compile_expr(second_index, instructions, locals)?;
            for other_index in index {
                instructions.push(OpCode::Concat, other_index.line_col());
                instructions.push(
                    OpCode::GetGlobal(SpecialVar::Subsep as u32),
                    other_index.line_col(),
                );
                instructions.push(OpCode::Concat, other_index.line_col());
                self.compile_expr(other_index, instructions, locals)?;
            }
            instructions.push(OpCode::Concat, second_index_line_col);
        }
        Ok(())
    }

    fn compile_binary_expr(
        &self,
        expr: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let expr = first_child(expr);
        match expr.as_rule() {
            Rule::simple_binary_expr | Rule::simple_binary_print_expr => {
                let expr = self.compile_simple_binary_expr(expr.into_inner(), locals)?;
                instructions.extend(expr.instructions);
            }
            Rule::multidimensional_in => {
                let mut inner = expr.into_inner();
                let index = inner.next().unwrap();
                let name = inner.next().unwrap();
                let get_instruction = self
                    .get_var(name.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(name.as_span(), msg))?;
                instructions.push(get_instruction, name.line_col());
                self.compile_array_index(index.into_inner(), instructions, locals)?;
                instructions.push(OpCode::In, name.line_col());
            }
            other => {
                unreachable!("encountered {:?} while compiling binary expression", other)
            }
        }
        Ok(())
    }

    fn compile_input_function(
        &self,
        expr: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let input_function = first_child(expr);
        let line_col = input_function.line_col();
        match input_function.as_rule() {
            Rule::simple_getline => {
                if let Some(lvalue) = input_function.into_inner().next() {
                    self.compile_lvalue(lvalue, instructions, locals)?;
                    lvalue_to_scalar_ref(&mut instructions.opcodes);
                } else {
                    instructions.extend(Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushZero, OpCode::FieldRef],
                        line_col,
                    ));
                }
                instructions.push(
                    OpCode::CallBuiltin {
                        function: BuiltinFunction::GetLine,
                        argc: 1,
                    },
                    line_col,
                );
            }
            Rule::getline_from_file => {
                let mut inner = input_function.into_inner();
                let lvalue = inner.next().unwrap();
                let file = if lvalue.as_rule() == Rule::lvalue {
                    self.compile_lvalue(lvalue, instructions, locals)?;
                    lvalue_to_scalar_ref(&mut instructions.opcodes);
                    inner.next().unwrap()
                } else {
                    instructions.extend(Instructions::from_instructions_and_line_col(
                        vec![OpCode::PushZero, OpCode::FieldRef],
                        line_col,
                    ));
                    lvalue
                };
                self.compile_expr(file, instructions, locals)?;
                instructions.push(
                    OpCode::CallBuiltin {
                        function: BuiltinFunction::GetLineFromFile,
                        argc: 2,
                    },
                    line_col,
                );
            }
            Rule::getline_from_pipe => {
                let mut inner = input_function.into_inner();
                let unpiped_expr = inner.next().unwrap();
                let mut lvalues = Vec::new();
                for piped_getline in inner {
                    lvalues.push(piped_getline.into_inner().next());
                }
                let getline_count = lvalues.len();
                for lvalue in lvalues.into_iter().rev() {
                    if let Some(lvalue) = lvalue {
                        self.compile_lvalue(lvalue.clone(), instructions, locals)?;
                        lvalue_to_scalar_ref(&mut instructions.opcodes);
                    } else {
                        instructions.extend(Instructions::from_instructions_and_line_col(
                            vec![OpCode::PushZero, OpCode::FieldRef],
                            line_col,
                        ));
                    }
                }
                self.compile_expr(unpiped_expr, instructions, locals)?;
                for _ in 0..getline_count {
                    instructions.push(
                        OpCode::CallBuiltin {
                            function: BuiltinFunction::GetLineFromPipe,
                            argc: 2,
                        },
                        line_col,
                    );
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn compile_expr(
        &self,
        expr: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let expr = first_child(expr);
        match expr.as_rule() {
            Rule::assignment => {
                let mut inner = expr.into_inner();
                self.compile_lvalue(inner.next().unwrap(), instructions, locals)?;
                lvalue_to_scalar_ref(&mut instructions.opcodes);
                let assignment_op = first_child(inner.next().unwrap());
                if assignment_op.as_rule() != Rule::assign {
                    instructions.push(OpCode::Dup, assignment_op.line_col());
                    self.compile_expr(inner.next().unwrap(), instructions, locals)?;
                    match assignment_op.as_rule() {
                        Rule::add_assign => {
                            instructions.push(OpCode::Add, assignment_op.line_col())
                        }
                        Rule::sub_assign => {
                            instructions.push(OpCode::Sub, assignment_op.line_col())
                        }
                        Rule::mul_assign => {
                            instructions.push(OpCode::Mul, assignment_op.line_col())
                        }
                        Rule::div_assign => {
                            instructions.push(OpCode::Div, assignment_op.line_col())
                        }
                        Rule::mod_assign => {
                            instructions.push(OpCode::Mod, assignment_op.line_col())
                        }
                        Rule::pow_assign => {
                            instructions.push(OpCode::Pow, assignment_op.line_col())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    self.compile_expr(inner.next().unwrap(), instructions, locals)?;
                }

                instructions.push(OpCode::Assign, assignment_op.line_col());
            }
            Rule::ternary_expr | Rule::ternary_print_expr => {
                let line_col = expr.line_col();
                let mut inner = expr.into_inner();
                self.compile_binary_expr(inner.next().unwrap(), instructions, locals)?;
                let mut true_expr_instructions = Instructions::default();
                self.compile_expr(inner.next().unwrap(), &mut true_expr_instructions, locals)?;
                instructions.push(
                    OpCode::JumpIfFalse(true_expr_instructions.len() as i32 + 2),
                    line_col,
                );
                instructions.extend(true_expr_instructions);
                let mut false_expr_instructions = Instructions::default();
                self.compile_expr(inner.next().unwrap(), &mut false_expr_instructions, locals)?;
                instructions.push(
                    OpCode::Jump(false_expr_instructions.len() as i32 + 1),
                    line_col,
                );
                instructions.extend(false_expr_instructions);
            }
            Rule::binary_expr | Rule::binary_print_expr => {
                self.compile_binary_expr(expr, instructions, locals)?;
            }
            Rule::input_function | Rule::unpiped_input_function => {
                self.compile_input_function(expr, instructions, locals)?;
            }
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
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let stmt = first_child(simple_stmt);
        let stmt_line_col = stmt.line_col();
        match stmt.as_rule() {
            Rule::array_delete => {
                let mut inner = stmt.into_inner();
                let name = inner.next().unwrap();
                let get_instruction = self
                    .get_var(name.as_str(), locals)
                    .map_err(|msg| pest_error_from_span(name.as_span(), msg))?;
                instructions.push(get_instruction, stmt_line_col);
                if let Some(index) = inner.next() {
                    self.compile_expr(index, instructions, locals)?;
                    instructions.push(OpCode::DeleteElement, stmt_line_col);
                } else {
                    instructions.push(OpCode::ClearArray, stmt_line_col);
                }
            }
            Rule::expr => {
                self.compile_expr(stmt, instructions, locals)?;
                instructions.push(OpCode::Pop, stmt_line_col);
            }
            Rule::print_stmt => {
                let mut inner = stmt.into_inner();
                let print = inner.next().unwrap();
                let mut print_function;
                let mut argc;
                match print.as_rule() {
                    Rule::simple_print
                    | Rule::print_call
                    | Rule::simple_printf
                    | Rule::printf_call => {
                        let is_printf =
                            matches!(print.as_rule(), Rule::printf_call | Rule::simple_printf);
                        let expressions = print.into_inner();
                        argc = expressions.len() as u16;
                        if expressions.len() == 0 {
                            // if it has no arguments it cannot be printf
                            assert!(!is_printf);
                            print_function = BuiltinFunction::Print;
                            instructions.push(OpCode::PushZero, stmt_line_col);
                            instructions.push(OpCode::GetField, stmt_line_col);
                            argc = 1;
                        } else {
                            for expr in expressions {
                                self.compile_expr(expr, instructions, locals)?;
                            }
                            if is_printf {
                                print_function = BuiltinFunction::Printf;
                            } else {
                                print_function = BuiltinFunction::Print;
                            }
                        }
                    }
                    _ => unreachable!(),
                }
                if let Some(output_redirection) = inner.next() {
                    match output_redirection.as_rule() {
                        Rule::truncate => {
                            if print_function == BuiltinFunction::Print {
                                print_function = BuiltinFunction::RedirectedPrintTruncate
                            } else {
                                print_function = BuiltinFunction::RedirectedPrintfTruncate
                            }
                        }
                        Rule::append => {
                            if print_function == BuiltinFunction::Print {
                                print_function = BuiltinFunction::RedirectedPrintAppend
                            } else {
                                print_function = BuiltinFunction::RedirectedPrintfAppend
                            }
                        }
                        Rule::pipe => {
                            if print_function == BuiltinFunction::Print {
                                print_function = BuiltinFunction::RedirectedPrintPipe
                            } else {
                                print_function = BuiltinFunction::RedirectedPrintfPipe
                            }
                        }
                        _ => unreachable!(),
                    }
                    let expr = first_child(output_redirection);
                    self.compile_expr(expr, instructions, locals)?;
                    argc += 1;
                }
                instructions.push(
                    OpCode::CallBuiltin {
                        function: print_function,
                        argc,
                    },
                    stmt_line_col,
                );
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
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = do_while.into_inner();
        let start_index = instructions.len();

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;

        let condition = inner.next().unwrap();
        let condition_line_col = condition.line_col();
        self.compile_expr(condition, instructions, locals)?;
        instructions.push(
            OpCode::JumpIfTrue(distance(instructions.len(), start_index)),
            condition_line_col,
        );

        Ok(())
    }

    fn compile_for_each(
        &mut self,
        for_each_stmt: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = for_each_stmt.into_inner();

        let iter_var = inner.next().unwrap();
        let iter_var_get_stmt = self
            .get_var(iter_var.as_str(), locals)
            .map_err(|msg| pest_error_from_span(iter_var.as_span(), msg))?;
        instructions.push(iter_var_get_stmt, iter_var.line_col());
        lvalue_to_scalar_ref(&mut instructions.opcodes);

        let array_var = inner.next().unwrap();
        let array_var_line_col = array_var.line_col();
        let array_var_get_stmt = self
            .get_var(array_var.as_str(), locals)
            .map_err(|msg| pest_error_from_span(array_var.as_span(), msg))?;

        match array_var_get_stmt {
            OpCode::GetGlobal(global_index) => instructions.push(
                OpCode::CreateGlobalIterator(global_index),
                array_var_line_col,
            ),
            OpCode::GetLocal(local_index) => {
                instructions.push(OpCode::CreateLocalIterator(local_index), array_var_line_col)
            }
            _ => unreachable!(),
        }

        let iter_deref_location = instructions.len();
        instructions.push(OpCode::Invalid, array_var_line_col);

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;

        instructions.push(
            OpCode::Jump(distance(instructions.len(), iter_deref_location)),
            array_var_line_col,
        );

        instructions.opcodes[iter_deref_location] =
            OpCode::AdvanceIterOrJump(distance(iter_deref_location, instructions.len()));

        Ok(())
    }

    fn compile_for(
        &mut self,
        for_stmt: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = for_stmt.into_inner();

        self.loop_stack.push(LoopStubs::default());

        let init = inner.next().unwrap();
        self.compile_simple_statement(init, instructions, locals)?;

        let condition_start = instructions.len();
        let condition = inner.next().unwrap();
        let condition_line_col = condition.line_col();
        self.compile_expr(condition, instructions, locals)?;
        let for_jump_index = instructions.len();
        instructions.push(OpCode::Invalid, condition_line_col);

        let update = inner.next().unwrap();
        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;
        let update_start = instructions.len();
        self.compile_simple_statement(update, instructions, locals)?;
        instructions.push(
            OpCode::Jump(distance(instructions.len(), condition_start)),
            condition_line_col,
        );
        instructions.opcodes[for_jump_index] =
            OpCode::JumpIfFalse(distance(for_jump_index, instructions.len()));

        let loop_stubs = self.loop_stack.pop().unwrap();
        for stub in loop_stubs.break_stubs {
            instructions.opcodes[stub] = OpCode::Jump(distance(stub, instructions.len()));
        }
        for stub in loop_stubs.continue_stubs {
            instructions.opcodes[stub] = OpCode::Jump(distance(stub, update_start));
        }

        Ok(())
    }

    fn compile_while(
        &mut self,
        while_stmt: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = while_stmt.into_inner();

        self.loop_stack.push(LoopStubs::default());

        let condition_start = instructions.len();
        let condition = inner.next().unwrap();
        let condition_line_col = condition.line_col();
        self.compile_expr(condition, instructions, locals)?;
        let while_jump_index = instructions.len();
        instructions.push(OpCode::Invalid, condition_line_col);

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;
        instructions.push(
            OpCode::Jump(distance(instructions.len(), condition_start)),
            condition_line_col,
        );

        instructions.opcodes[while_jump_index] =
            OpCode::JumpIfFalse(distance(while_jump_index, instructions.len()));

        let loop_stubs = self.loop_stack.pop().unwrap();
        for stub in loop_stubs.break_stubs {
            instructions.opcodes[stub] = OpCode::Jump(distance(stub, instructions.len()));
        }
        for stub in loop_stubs.continue_stubs {
            instructions.opcodes[stub] = OpCode::Jump(distance(stub, condition_start));
        }
        Ok(())
    }

    fn compile_if(
        &mut self,
        if_stmt: Pair<Rule>,
        instructions: &mut Instructions,
        locals: &LocalMap,
    ) -> Result<(), PestError> {
        let mut inner = if_stmt.into_inner();

        let condition = inner.next().unwrap();
        let condition_line_col = condition.line_col();
        self.compile_expr(condition, instructions, locals)?;

        let if_jump_index = instructions.len();
        instructions.push(OpCode::Invalid, condition_line_col);

        let body = inner.next().unwrap();
        self.compile_stmt(body, instructions, locals)?;

        if let Some(else_body) = inner.next() {
            let else_jump_index = instructions.len();
            instructions.push(OpCode::Invalid, else_body.line_col());
            instructions.opcodes[if_jump_index] =
                OpCode::JumpIfFalse(distance(if_jump_index, instructions.len()));
            self.compile_stmt(else_body, instructions, locals)?;
            instructions.opcodes[else_jump_index] =
                OpCode::Jump(distance(else_jump_index, instructions.len()));
        } else {
            instructions.opcodes[if_jump_index] =
                OpCode::JumpIfFalse(distance(if_jump_index, instructions.len()));
        }

        Ok(())
    }

    fn compile_action(
        &mut self,
        action: Pair<Rule>,
        instructions: &mut Instructions,
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
        instructions: &mut Instructions,
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
            Rule::nextfile => {
                instructions.push(OpCode::NextFile, stmt.line_col());
                Ok(())
            }
            Rule::next => {
                instructions.push(OpCode::Next, stmt.line_col());
                Ok(())
            }
            Rule::break_stmt => {
                if let Some(loop_stubs) = self.loop_stack.last_mut() {
                    loop_stubs.break_stubs.push(instructions.len());
                    instructions.push(OpCode::Invalid, stmt.line_col());
                    Ok(())
                } else {
                    Err(pest_error_from_span(
                        stmt.as_span(),
                        "break statement outside of loop".to_string(),
                    ))
                }
            }
            Rule::continue_stmt => {
                if let Some(loop_stubs) = self.loop_stack.last_mut() {
                    loop_stubs.continue_stubs.push(instructions.len());
                    instructions.push(OpCode::Invalid, stmt.line_col());
                    Ok(())
                } else {
                    Err(pest_error_from_span(
                        stmt.as_span(),
                        "continue statement outside of loop".to_string(),
                    ))
                }
            }
            Rule::exit_stmt => {
                let stmt_line_col = stmt.line_col();
                if let Some(expr) = stmt.into_inner().next() {
                    self.compile_expr(expr, instructions, locals)?;
                } else {
                    instructions.push(OpCode::PushZero, stmt_line_col);
                }
                instructions.push(OpCode::Exit, stmt_line_col);
                Ok(())
            }
            Rule::return_stmt => {
                if !self.in_function {
                    return Err(pest_error_from_span(
                        stmt.as_span(),
                        "return statement outside of function".to_string(),
                    ));
                }
                let stmt_line_col = stmt.line_col();
                if let Some(expr) = stmt.into_inner().next() {
                    self.compile_expr(expr, instructions, locals)?;
                } else {
                    instructions.push(OpCode::PushUninitializedScalar, stmt_line_col);
                }
                instructions.push(OpCode::Return, stmt_line_col);
                Ok(())
            }
            Rule::do_while => self.compile_do_while(stmt, instructions, locals),
            _ => unreachable!("encountered {:?} while compiling statement", stmt.as_rule()),
        }
    }

    fn compile_normal_pattern(
        &mut self,
        pattern: Pair<Rule>,
        file: Rc<str>,
    ) -> Result<Pattern, PestError> {
        let pattern = first_child(pattern);
        match pattern.as_rule() {
            Rule::expr => {
                let mut instructions = Instructions::default();
                self.compile_expr(pattern, &mut instructions, &HashMap::new())?;
                Ok(Pattern::Expr(instructions.into_action(file)))
            }
            Rule::range_pattern => {
                let mut inner = pattern.into_inner();

                let start = inner.next().unwrap();
                let mut start_instructions = Instructions::default();
                self.compile_expr(start, &mut start_instructions, &HashMap::new())?;

                let end = inner.next().unwrap();
                let mut end_instructions = Instructions::default();
                self.compile_expr(end, &mut end_instructions, &HashMap::new())?;

                Ok(Pattern::Range {
                    start: start_instructions.into_action(file.clone()),
                    end: end_instructions.into_action(file),
                })
            }
            _ => unreachable!(
                "encountered {:?} while compiling pattern",
                pattern.as_rule()
            ),
        }
    }

    fn compile_rule(&mut self, rule: Pair<Rule>, file: Rc<str>) -> Result<AwkRule, PestError> {
        let rule = first_child(rule);
        match rule.as_rule() {
            Rule::action => {
                let mut instructions = Instructions::default();
                self.compile_action(rule, &mut instructions, &HashMap::new())?;
                Ok(AwkRule {
                    pattern: Pattern::All,
                    action: instructions.into_action(file),
                })
            }
            Rule::pattern_and_action => {
                let mut inner = rule.into_inner();
                let pattern = self.compile_normal_pattern(inner.next().unwrap(), file.clone())?;
                let action = inner.next().unwrap();
                let mut instructions = Instructions::default();
                let locals = HashMap::new();
                self.compile_action(action, &mut instructions, &locals)?;
                Ok(AwkRule {
                    pattern,
                    action: instructions.into_action(file),
                })
            }
            Rule::normal_pattern => {
                let rule_line_col = rule.line_col();
                let pattern = self.compile_normal_pattern(rule, file.clone())?;
                let instructions = Instructions::from_instructions_and_line_col(
                    vec![
                        OpCode::PushZero,
                        OpCode::GetField,
                        OpCode::CallBuiltin {
                            function: BuiltinFunction::Print,
                            argc: 1,
                        },
                    ],
                    rule_line_col,
                );
                Ok(AwkRule {
                    pattern,
                    action: instructions.into_action(file),
                })
            }
            _ => unreachable!("encountered {:?} while compiling rule", rule.as_rule()),
        }
    }

    fn compile_function_definition(
        &mut self,
        function: Pair<Rule>,
        file: Rc<str>,
    ) -> Result<Function, PestError> {
        let mut inner = function.into_inner();
        let name = inner.next().unwrap();
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
        let mut instructions = Instructions::default();
        self.in_function = true;
        self.compile_action(body, &mut instructions, &param_map)?;
        self.in_function = false;

        // ensure that functions always return
        if !matches!(instructions.opcodes.last(), Some(OpCode::Return)) {
            let name_line_col = name.line_col();
            instructions.push(OpCode::PushUninitializedScalar, name_line_col);
            instructions.push(OpCode::Return, name_line_col);
        }

        Ok(Function {
            name: name.as_str().into(),
            parameters_count,
            instructions: instructions.opcodes,
            debug_info: DebugInfo {
                file,
                source_locations: instructions.source_locations,
            },
        })
    }

    fn declare_program_functions(
        &mut self,
        program: Pairs<Rule>,
        filename: &str,
        errors: &mut Vec<PestError>,
    ) {
        for item in program {
            if item.as_rule() == Rule::function_definition {
                let mut inner = item.into_inner();
                let name = inner.next().unwrap();
                let maybe_parameter_list = inner.next().unwrap();
                let parameter_count = if maybe_parameter_list.as_rule() == Rule::param_list {
                    maybe_parameter_list.into_inner().count() as u32
                } else {
                    0
                };
                let function_id = post_increment(&self.last_global_function_id);
                let previous_value = self.names.borrow_mut().insert(
                    name.as_str().to_string(),
                    GlobalName::Function {
                        id: function_id,
                        parameter_count,
                    },
                );
                if previous_value.is_some() {
                    let error = improve_error(
                        pest_error_from_span(
                            name.as_span(),
                            format!("function '{}' is defined multiple times", name),
                        ),
                        filename,
                    );
                    errors.push(error);
                }
            }
        }
    }
}

fn location_end(loc: &InputLocation) -> usize {
    match loc {
        InputLocation::Pos(p) => *p,
        InputLocation::Span((_, end)) => *end,
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub struct CompilerErrors {
    errors: Vec<PestError>,
}

impl std::fmt::Display for CompilerErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

fn next_checkpoint(text: &str) -> Option<usize> {
    text.find('}')
        .map(|p| text.len().min(p + 1))
        .into_iter()
        .chain(text.find("BEGIN"))
        .chain(text.find("END"))
        .chain(text.find("function"))
        .min()
}

fn improve_error(error: PestError, file: &str) -> PestError {
    if !file.is_empty() {
        error.with_path(file)
    } else {
        error
    }
}

fn gather_errors(first_error: PestError, source: &str, errors: &mut Vec<PestError>, file: &str) {
    let first_error_end = location_end(&first_error.location);

    errors.push(improve_error(first_error, file));
    let mut parsing_start = first_error_end;

    while let Some(checkpoint_offset) = next_checkpoint(&source[parsing_start..]) {
        parsing_start += checkpoint_offset;
        match AwkParser::parse(Rule::program, &source[parsing_start..]) {
            Ok(_) => break,
            Err(err) => errors.push(improve_error(err, file)),
        }
    }
}

pub struct SourceFile {
    pub filename: String,
    pub contents: String,
}

impl SourceFile {
    pub fn stdin(contents: String) -> Self {
        Self {
            filename: "".to_string(),
            contents,
        }
    }
}

pub fn compile_program(sources: &[SourceFile]) -> Result<Program, CompilerErrors> {
    let mut parsed_sources = Vec::new();
    let mut errors = Vec::new();
    for source_file in sources {
        let filename: Rc<str> = source_file.filename.clone().into();
        match AwkParser::parse(Rule::program, &source_file.contents) {
            Ok(mut program) => {
                let program = program.next().unwrap();
                parsed_sources.push((filename, program.into_inner()));
            }
            Err(err) => {
                gather_errors(err, &source_file.contents, &mut errors, &filename);
            }
        };
    }

    let mut compiler = Compiler::default();
    for (filename, program_iter) in &parsed_sources {
        compiler.declare_program_functions(program_iter.clone(), filename, &mut errors);
    }

    let mut begin_actions = Vec::new();
    let mut rules = Vec::new();
    let mut end_actions = Vec::new();
    let mut functions = Vec::new();
    for (filename, program_iter) in parsed_sources {
        for item in program_iter {
            match item.as_rule() {
                Rule::begin_action | Rule::end_action => {
                    let is_begin_action = item.as_rule() == Rule::begin_action;
                    let mut instructions = Instructions::default();
                    let result = compiler.compile_action(
                        first_child(item),
                        &mut instructions,
                        &HashMap::new(),
                    );
                    if let Err(err) = result {
                        errors.push(improve_error(err, &filename));
                    }
                    if is_begin_action {
                        begin_actions.push(instructions.into_action(filename.clone()));
                    } else {
                        end_actions.push(instructions.into_action(filename.clone()));
                    }
                }
                Rule::rule => match compiler.compile_rule(item, filename.clone()) {
                    Ok(rule) => rules.push(rule),
                    Err(err) => errors.push(improve_error(err, &filename)),
                },
                Rule::function_definition => {
                    match compiler.compile_function_definition(item, filename.clone()) {
                        Ok(function) => functions.push(function),
                        Err(err) => errors.push(improve_error(err, &filename)),
                    }
                }
                Rule::EOI => {}
                _ => unreachable!("encontered {:?} while compiling program", item.as_rule()),
            }
        }
    }

    let globals = compiler
        .names
        .into_inner()
        .into_iter()
        .filter_map(|(k, v)| match v {
            GlobalName::Variable(id) => Some((k, id)),
            _ => None,
        })
        .collect();

    if errors.is_empty() {
        Ok(Program {
            constants: compiler.constants.into_inner(),
            begin_actions,
            rules,
            end_actions,
            functions,
            globals_count: compiler.last_global_var_id.get() as usize,
            globals,
        })
    } else {
        Err(CompilerErrors { errors })
    }
}

/// Returns true if the given string is a valid number token.
pub fn is_valid_number(s: &str) -> bool {
    AwkParser::parse(Rule::number, s).is_ok()
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::regex::regex_from_str;

    const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

    fn compile_expr(expr: &str) -> (Vec<OpCode>, Vec<Constant>) {
        let mut program = compile_program(&[SourceFile::stdin(format!("BEGIN {{ {} }}", expr))])
            .expect("error compiling expression");
        // remove OpCode::Pop
        program.begin_actions[0].instructions.pop();
        (
            program.begin_actions[0].instructions.clone(),
            program.constants,
        )
    }

    fn compile_stmt(stmt: &str) -> (Vec<OpCode>, Vec<Constant>) {
        let program = compile_program(&[SourceFile::stdin(format!("BEGIN {{ {} }}", stmt))])
            .expect("error compiling statement");
        (
            program.begin_actions[0].instructions.clone(),
            program.constants,
        )
    }

    fn compile_correct_program(text: &str) -> Program {
        compile_program(&[SourceFile::stdin(text.to_string())]).expect("error compiling program")
    }

    fn does_not_compile(text: &str) {
        compile_program(&[SourceFile::stdin(text.to_string())])
            .expect_err("expected error compiling program");
    }

    #[test]
    fn test_compile_empty_program() {
        let program = compile_correct_program("");
        assert!(program.constants.is_empty());
        assert!(program.begin_actions.is_empty());
        assert!(program.rules.is_empty());
        assert!(program.end_actions.is_empty());
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_compile_empty_begin() {
        let program = compile_correct_program("BEGIN {}");
        assert!(program.constants.is_empty());
        assert_eq!(program.begin_actions.len(), 1);
        assert!(program.begin_actions[0].instructions.is_empty());
        assert!(program.rules.is_empty());
        assert!(program.end_actions.is_empty());
        assert!(program.functions.is_empty());
    }

    #[test]
    fn test_compile_empty_end() {
        let program = compile_correct_program("END {}");
        assert!(program.constants.is_empty());
        assert!(program.begin_actions.is_empty());
        assert!(program.rules.is_empty());
        assert_eq!(program.end_actions.len(), 1);
        assert!(program.end_actions[0].instructions.is_empty());
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
        let (_, constants) = compile_expr(r#""""#);
        assert_eq!(constants, vec!["".into()]);

        let (_, constants) = compile_expr(r#""hello""#);
        assert_eq!(constants, vec!["hello".into()]);

        let (_, constants) = compile_expr(r#""\"""#);
        assert_eq!(constants, vec![Constant::from("\"")]);

        let (_, constants) = compile_expr(r#""\/""#);
        assert_eq!(constants, vec![Constant::from("/")]);

        let (_, constants) = compile_expr(r#""\a""#);
        assert_eq!(constants, vec![Constant::from("\x07")]);

        let (_, constants) = compile_expr(r#""\b""#);
        assert_eq!(constants, vec![Constant::from("\x08")]);

        let (_, constants) = compile_expr(r#""\f""#);
        assert_eq!(constants, vec![Constant::from("\x0C")]);

        let (_, constants) = compile_expr(r#""\n""#);
        assert_eq!(constants, vec![Constant::from("\n")]);

        let (_, constants) = compile_expr(r#""\r""#);
        assert_eq!(constants, vec![Constant::from("\r")]);

        let (_, constants) = compile_expr(r#""\t""#);
        assert_eq!(constants, vec![Constant::from("\t")]);

        let (_, constants) = compile_expr(r#""\v""#);
        assert_eq!(constants, vec![Constant::from("\x0B")]);

        let (_, constants) = compile_expr(r#""\\""#);
        assert_eq!(constants, vec![Constant::from("\\")]);

        let (_, constants) = compile_expr(r#""\7""#);
        assert_eq!(constants, vec![Constant::from("\x07")]);

        let (_, constants) = compile_expr(r#""\41""#);
        assert_eq!(constants, vec![Constant::from("!")]);

        let (_, constants) = compile_expr(r#""\142""#);
        assert_eq!(constants, vec![Constant::from("b")]);

        let (_, constants) = compile_expr(r#""hello\nworld""#);
        assert_eq!(constants, vec![Constant::from("hello\nworld")]);

        let (_, constants) = compile_expr(r#""hello\tworld""#);
        assert_eq!(constants, vec!["hello\tworld".into()]);

        let (_, constants) = compile_expr(r#""hello\\world""#);
        assert_eq!(constants, vec![Constant::from("hello\\world")]);

        let (_, constants) = compile_expr(r#""hello\"world""#);
        assert_eq!(constants, vec![Constant::from(r#"hello"world"#)]);

        let (_, constants) = compile_expr(r#""hello\41world""#);
        assert_eq!(constants, vec![Constant::from("hello!world")]);

        let (_, constants) = compile_expr(r#""hello\141world""#);
        assert_eq!(constants, vec![Constant::from("helloaworld")]);
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
            vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreInc]
        );

        let (instructions, _) = compile_expr("--a");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreDec]
        );

        let (instructions, _) = compile_expr("a++");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostInc]
        );

        let (instructions, _) = compile_expr("a--");
        assert_eq!(
            instructions,
            vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostDec]
        );

        let (instructions, _) = compile_expr("++a[0]");
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetRef,
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
            vec![Constant::from("hello"), Constant::from("world"),]
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
                Constant::from("hello"),
                Constant::Number(1.0),
                Constant::from("world"),
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
            vec![Constant::from("hello"), Constant::Number(1.0)]
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
            vec![Constant::Number(1.0), Constant::from("hello")]
        );
    }

    #[test]
    fn test_compile_comp_op() {
        let (instructions, constants) = compile_expr(r#"1 < "x""#);
        assert_eq!(
            instructions,
            vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Lt]
        );
        assert_eq!(constants, vec![Constant::Number(1.0), Constant::from("x")]);

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
            vec![Constant::from("str"), Constant::Number(2.0)]
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::In
            ]
        );
        assert_eq!(constants, vec![Constant::from("a")]);
    }

    #[test]
    fn test_compile_multidimensional_in_operator() {
        let (instructions, constants) = compile_expr(r#"(1, 2) in map"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(1),
                OpCode::Concat,
                OpCode::In
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr(r#"(1, "str", 3) in map"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(1),
                OpCode::Concat,
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(2),
                OpCode::Concat,
                OpCode::In
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::from("str"),
                Constant::Number(3.0)
            ]
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
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
                Constant::from("hello"),
                Constant::Regex(Rc::new(regex_from_str("hello")))
            ]
        )
    }

    #[test]
    #[cfg_attr(miri, ignore)]
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
                Constant::from("test"),
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
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Jump(2),
                OpCode::PushZero,
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
                OpCode::JumpIfFalse(5),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::Ge,
                OpCode::Jump(2),
                OpCode::PushZero,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("x"),
                Constant::from("y"),
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
                OpCode::Jump(2),
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
                OpCode::Jump(2),
                OpCode::PushOne,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("x"),
                Constant::from("y"),
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
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Eq,
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Jump(8),
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
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
                Constant::from("one"),
                Constant::Number(2.0),
                Constant::from("two"),
                Constant::from("many"),
            ]
        );
    }

    #[test]
    fn test_compile_simple_variable_assignment() {
        let (instructions, constants) = compile_expr("a = 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);

        let (instructions, constants) = compile_expr("a = b = 2");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::Dup,
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
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
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetRef,
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
    fn compile_multidimensional_array_index() {
        let (instructions, constants) = compile_expr("a[1, 2]");
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(1),
                OpCode::Concat,
                OpCode::IndexArrayGetValue,
            ]
        );
        assert_eq!(
            constants,
            vec![Constant::Number(1.0), Constant::Number(2.0)]
        );

        let (instructions, constants) = compile_expr("a[1, 2, \"test\"]");
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(1),
                OpCode::Concat,
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Concat,
                OpCode::PushConstant(2),
                OpCode::Concat,
                OpCode::IndexArrayGetValue,
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("test")
            ]
        );
    }

    #[test]
    fn compile_field_var_assignment() {
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
    fn compile_field_var_compairisons() {
        let (instructions, constants) = compile_expr("$1 == 1");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::GetField,
                OpCode::PushConstant(1),
                OpCode::Eq,
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
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
                OpCode::Pop,
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfFalse(7),
                OpCode::PushConstant(2),
                OpCode::Pop,
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
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
    fn test_compile_break_inside_while_loop() {
        let (instructions, _) = compile_stmt("while (1) { 1; break; }");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(5),
                OpCode::PushConstant(1),
                OpCode::Pop,
                OpCode::Jump(2),
                OpCode::Jump(-5),
            ]
        );
    }

    #[test]
    fn test_compile_continue_in_while_loop() {
        let (instructions, _) = compile_stmt("while (1) { 1; continue; }");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::JumpIfFalse(5),
                OpCode::PushConstant(1),
                OpCode::Pop,
                OpCode::Jump(-4),
                OpCode::Jump(-5),
            ]
        );
    }

    #[test]
    fn test_compile_break_inside_for_loop() {
        let (instructions, _) = compile_stmt("for (i = 0; i < 10; i++) { 1; break; }");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
                OpCode::Pop,
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfFalse(8),
                OpCode::PushConstant(2),
                OpCode::Pop,
                OpCode::Jump(5),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PostInc,
                OpCode::Pop,
                OpCode::Jump(-10),
            ]
        );
    }

    #[test]
    fn test_compile_continue_in_for_loop() {
        let (instructions, _) = compile_stmt("for (i = 0; i < 10; i++) { 1; continue; }");
        assert_eq!(
            instructions,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::Assign,
                OpCode::Pop,
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(1),
                OpCode::Lt,
                OpCode::JumpIfFalse(8),
                OpCode::PushConstant(2),
                OpCode::Pop,
                OpCode::Jump(1),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PostInc,
                OpCode::Pop,
                OpCode::Jump(-10),
            ]
        );
    }

    #[test]
    fn test_compile_next() {
        let (instructions, _) = compile_stmt("next;");
        assert_eq!(instructions, vec![OpCode::Next]);
    }

    #[test]
    fn test_compile_nextfile() {
        let (instructions, _) = compile_stmt("nextfile;");
        assert_eq!(instructions, vec![OpCode::NextFile]);
    }

    #[test]
    fn test_compile_exit() {
        let (instructions, _) = compile_stmt("exit;");
        assert_eq!(instructions, vec![OpCode::PushZero, OpCode::Exit]);

        let (instructions, _) = compile_stmt("exit 1;");
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
    fn test_compile_delete_element() {
        let (instructions, _) = compile_stmt("delete a[1];");
        assert_eq!(
            instructions,
            vec![
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::DeleteElement,
            ]
        );
    }

    #[test]
    fn test_compile_clear_array() {
        let (instructions, _) = compile_stmt("delete a");
        assert_eq!(
            instructions,
            vec![OpCode::GetGlobal(FIRST_GLOBAL_VAR), OpCode::ClearArray]
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
                Constant::from("number"),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("and"),
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
        assert_eq!(constant, vec![Constant::from("hello")]);

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
                Constant::from("hello"),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("and"),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_redirected_simple_print() {
        let (instructions, constant) = compile_stmt("print 1 > \"file\";");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintTruncate,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::Number(1.0), Constant::from("file"),]
        );

        let (instructions, constant) = compile_stmt("print 1 >> \"file\";");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintAppend,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::Number(1.0), Constant::from("file"),]
        );

        let (instructions, constant) = compile_stmt(r#"print 1 | "bash";"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintPipe,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::Number(1.0), Constant::from("bash"),]
        );
    }

    #[test]
    fn test_compile_simple_printf() {
        let (instructions, constant) = compile_stmt("printf \"test\";");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Printf,
                    argc: 1
                },
            ]
        );
        assert_eq!(constant, vec![Constant::from("test"),]);

        let (instructions, constant) =
            compile_stmt(r#"printf "%d, %d, %s, %.6f", 1, 2, "and", 3;"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Printf,
                    argc: 5
                },
            ]
        );
        assert_eq!(
            constant,
            vec![
                Constant::from("%d, %d, %s, %.6f"),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("and"),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_printf_call() {
        let (instructions, constant) = compile_stmt("printf (\"hello\");");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Printf,
                    argc: 1
                },
            ]
        );
        assert_eq!(constant, vec![Constant::from("hello")]);

        let (instructions, constants) =
            compile_stmt(r#"printf ("%g, %x, %s, %.6f", 1, 2, "and", 3);"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::PushConstant(4),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Printf,
                    argc: 5
                },
            ]
        );
        assert_eq!(
            constants,
            vec![
                Constant::from("%g, %x, %s, %.6f"),
                Constant::Number(1.0),
                Constant::Number(2.0),
                Constant::from("and"),
                Constant::Number(3.0),
            ]
        );
    }

    #[test]
    fn test_compile_redirected_simple_printf() {
        let (instructions, constant) = compile_stmt("printf \"test\" > \"file\";");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintfTruncate,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::from("test"), Constant::from("file"),]
        );

        let (instructions, constant) = compile_stmt("printf \"test\" >> \"file\";");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintfAppend,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::from("test"), Constant::from("file"),]
        );

        let (instructions, constant) = compile_stmt(r#"printf "test" | "bash";"#);
        assert_eq!(
            instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::RedirectedPrintfPipe,
                    argc: 2
                },
            ]
        );
        assert_eq!(
            constant,
            vec![Constant::from("test"), Constant::from("bash"),]
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
                OpCode::GetGlobal(FIRST_GLOBAL_VAR),
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
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetRef,
                OpCode::GetLocal(1),
                OpCode::GetLocal(2),
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
            program.begin_actions[0].instructions,
            vec![OpCode::Call(0), OpCode::Pop]
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
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
        assert_eq!(
            program.begin_actions[0].instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::Call(0),
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
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ]
        );
        assert_eq!(
            program.begin_actions[0].instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::PushUninitialized,
                OpCode::Call(0),
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
        assert!(matches!(
            &program.rules[0].pattern,
            Pattern::Expr(Action { instructions, .. }) if *instructions == [OpCode::PushConstant(0)]
        ));
        assert_eq!(
            program.rules[0].action.instructions,
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
        assert!(matches!(
            &program.rules[0].pattern,
            Pattern::Range { start, end } if start.instructions == [OpCode::PushConstant(0)] && end.instructions == [OpCode::PushConstant(1)]
        ));
        assert_eq!(
            program.rules[0].action.instructions,
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
            program.rules[0].action.instructions,
            vec![OpCode::PushConstant(0), OpCode::Pop,]
        );
        assert_eq!(program.constants, vec![Constant::Number(1.0)]);
    }

    #[test]
    fn compile_rule_without_action() {
        let program = compile_correct_program("1");
        assert_eq!(program.rules.len(), 1);
        assert!(matches!(
            &program.rules[0].pattern,
            Pattern::Expr(Action { instructions, .. }) if *instructions == [OpCode::PushConstant(0)]
        ));
        assert_eq!(
            program.rules[0].action.instructions,
            vec![
                OpCode::PushZero,
                OpCode::GetField,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 1
                }
            ]
        );
        assert_eq!(program.constants, vec![Constant::Number(1.0)]);
    }

    #[test]
    fn test_compile_print_with_no_args() {
        let (instructions, _) = compile_stmt("print;");
        assert_eq!(
            instructions,
            vec![
                OpCode::PushZero,
                OpCode::GetField,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Print,
                    argc: 1
                }
            ]
        );
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
            program.begin_actions[0].instructions,
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
    fn test_compile_builtin_string_functions() {
        let program = compile_correct_program(
            r#"
            BEGIN {
                gsub(/test/, "x");
                gsub(/y/, "x", s);
                index("a", "b");
                match("a", "b");
                split("test string", a);
                split("test string", a, /ere/);
                length;
                length();
                length("a");
                sprintf("a", 1);
                sprintf("a", 1, 2, 3, 4, 5);
                sub(/test/, "x");
                sub(/y/, "x", s);
                substr("a", 1);
                substr("a", 1, 2);
                tolower("A");
                toupper("a");
            }
        "#,
        );
        assert_eq!(
            program.begin_actions[0].instructions,
            vec![
                OpCode::PushZero,
                OpCode::FieldRef,
                OpCode::PushConstant(0),
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Gsub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(2),
                OpCode::PushConstant(3),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Gsub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(4),
                OpCode::PushConstant(5),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Index,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(6),
                OpCode::PushConstant(7),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Match,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::GetGlobal(FIRST_GLOBAL_VAR + 1),
                OpCode::PushConstant(8),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Split,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::GetGlobal(FIRST_GLOBAL_VAR + 1),
                OpCode::PushConstant(9),
                OpCode::PushConstant(10),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Split,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushZero,
                OpCode::GetField,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushZero,
                OpCode::GetField,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(11),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Length,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(12),
                OpCode::PushConstant(13),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sprintf,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(14),
                OpCode::PushConstant(15),
                OpCode::PushConstant(16),
                OpCode::PushConstant(17),
                OpCode::PushConstant(18),
                OpCode::PushConstant(19),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sprintf,
                    argc: 6,
                },
                OpCode::Pop,
                OpCode::PushZero,
                OpCode::FieldRef,
                OpCode::PushConstant(20),
                OpCode::PushConstant(21),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(22),
                OpCode::PushConstant(23),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Sub,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(24),
                OpCode::PushConstant(25),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Substr,
                    argc: 2,
                },
                OpCode::Pop,
                OpCode::PushConstant(26),
                OpCode::PushConstant(27),
                OpCode::PushConstant(28),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Substr,
                    argc: 3,
                },
                OpCode::Pop,
                OpCode::PushConstant(29),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::ToLower,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(30),
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
                fflush("file");
                fflush();
                system("ls");
            }
        "#,
        );
        assert_eq!(
            program.begin_actions[0].instructions,
            vec![
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::Close,
                    argc: 1,
                },
                OpCode::Pop,
                OpCode::PushConstant(1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::FFlush,
                    argc: 1
                },
                OpCode::Pop,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::FFlush,
                    argc: 0
                },
                OpCode::Pop,
                OpCode::PushConstant(2),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::System,
                    argc: 1,
                },
                OpCode::Pop,
            ]
        );
    }

    #[test]
    fn test_compile_special_variables() {
        let program = compile_correct_program(
            r#"
            BEGIN {
                ARGC
                ARGV
                CONVFMT
                ENVIRON
                FILENAME
                FNR
                FS
                NF
                NR
                OFMT
                OFS
                ORS
                RLENGTH
                RS
                RSTART
                SUBSEP
            }
        "#,
        );
        assert_eq!(
            program.begin_actions[0].instructions,
            vec![
                OpCode::GetGlobal(SpecialVar::Argc as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Argv as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Convfmt as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Environ as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Filename as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Fnr as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Fs as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Nf as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Nr as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Ofmt as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Ofs as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Ors as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Rlength as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Rs as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Rstart as u32),
                OpCode::Pop,
                OpCode::GetGlobal(SpecialVar::Subsep as u32),
                OpCode::Pop,
            ]
        );
    }

    #[test]
    fn test_compile_for_each_stmt() {
        let (program, constants) = compile_stmt("for (a in array) {1;}");
        assert_eq!(
            program,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::CreateGlobalIterator(FIRST_GLOBAL_VAR + 1),
                OpCode::AdvanceIterOrJump(4),
                OpCode::PushConstant(0),
                OpCode::Pop,
                OpCode::Jump(-3)
            ]
        );
        assert_eq!(constants, vec![Constant::Number(1.0)]);
    }

    #[test]
    fn test_compile_recursive_function() {
        let program = compile_correct_program(
            r#"
            function fib(n) {
                if (n <= 1) {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
        "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::Le,
                OpCode::JumpIfFalse(3),
                OpCode::GetLocal(0),
                OpCode::Return,
                OpCode::GetLocal(0),
                OpCode::PushConstant(1),
                OpCode::Sub,
                OpCode::Call(0),
                OpCode::GetLocal(0),
                OpCode::PushConstant(2),
                OpCode::Sub,
                OpCode::Call(0),
                OpCode::Add,
                OpCode::Return,
            ]
        );
    }

    #[test]
    fn test_compile_mutually_recursive_functions() {
        let program = compile_correct_program(
            r#"
            function is_even(n) {
                if (n == 0) {
                    return 1;
                }
                return is_odd(n - 1);
            }

            function is_odd(n) {
                if (n == 0) {
                    return 0;
                }
                return is_even(n - 1);
            }
        "#,
        );
        assert_eq!(
            program.functions[0].instructions,
            vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::Eq,
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(1),
                OpCode::Return,
                OpCode::GetLocal(0),
                OpCode::PushConstant(2),
                OpCode::Sub,
                OpCode::Call(1),
                OpCode::Return,
            ]
        );
        assert_eq!(
            program.functions[1].instructions,
            vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(3),
                OpCode::Eq,
                OpCode::JumpIfFalse(3),
                OpCode::PushConstant(4),
                OpCode::Return,
                OpCode::GetLocal(0),
                OpCode::PushConstant(5),
                OpCode::Sub,
                OpCode::Call(0),
                OpCode::Return,
            ]
        );
    }

    #[test]
    fn test_compile_simple_getline() {
        let (expr, _) = compile_expr("getline");
        assert_eq!(
            expr,
            vec![
                OpCode::PushZero,
                OpCode::FieldRef,
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLine,
                    argc: 1
                }
            ]
        );

        let (expr, _) = compile_expr("getline var");
        assert_eq!(
            expr,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLine,
                    argc: 1
                }
            ]
        )
    }

    #[test]
    fn test_compile_getline_from_file() {
        let (expr, _) = compile_expr("getline < \"file\"");
        assert_eq!(
            expr,
            vec![
                OpCode::PushZero,
                OpCode::FieldRef,
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromFile,
                    argc: 2
                }
            ]
        );

        let (expr, _) = compile_expr("getline var < \"file\"");
        assert_eq!(
            expr,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromFile,
                    argc: 2
                }
            ]
        );
    }

    #[test]
    fn test_compile_piped_getline() {
        let (expr, _) = compile_expr("\"command\" | getline");
        assert_eq!(
            expr,
            vec![
                OpCode::PushZero,
                OpCode::FieldRef,
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                }
            ]
        );

        let (expr, _) = compile_expr("\"command\" | getline var");
        assert_eq!(
            expr,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                }
            ]
        );

        let (expr, _) = compile_expr("getline var | getline var2");
        assert_eq!(
            expr,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLine,
                    argc: 1
                },
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                }
            ]
        );

        let (expr, _) = compile_expr("\"test\" | getline var | getline var2 | getline var3");
        assert_eq!(
            expr,
            vec![
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
                OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 2),
                OpCode::PushConstant(0),
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                },
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                },
                OpCode::CallBuiltin {
                    function: BuiltinFunction::GetLineFromPipe,
                    argc: 2
                }
            ]
        );
    }
}
