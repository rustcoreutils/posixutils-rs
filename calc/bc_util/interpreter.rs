//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::Write;
use std::rc::Rc;

use super::instructions::{
    BuiltinFunction, ConditionInstruction, ExprInstruction, Function, FunctionArgument, NamedExpr,
    Program, Register, StmtInstruction,
};
use super::number::Number;
use crate::bc_util::instructions::Variable;

#[derive(Debug)]
struct ErrorCall {
    function_name: char,
    line: usize,
    file: Rc<str>,
}

#[derive(Debug)]
pub struct ExecutionError {
    message: &'static str,
    call_stack: Vec<ErrorCall>,
    partial_output: String,
}

impl ExecutionError {
    fn add_call(mut self, function_name: char, line: usize, file: Rc<str>) -> Self {
        self.call_stack.push(ErrorCall {
            function_name,
            line,
            file,
        });
        self
    }

    fn global_source(mut self, line: usize, file: Rc<str>) -> Self {
        self.call_stack.push(ErrorCall {
            function_name: '\0',
            line,
            file,
        });
        self
    }

    pub fn partial_output(&self) -> &str {
        &self.partial_output
    }
}

impl From<&'static str> for ExecutionError {
    fn from(message: &'static str) -> Self {
        Self {
            message,
            call_stack: Vec::new(),
            partial_output: String::new(),
        }
    }
}

impl std::fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.call_stack.len() == 1 {
            let line = self.call_stack[0].line;
            let file = &self.call_stack[0].file;
            if file.is_empty() {
                return write!(f, "runtime error (line {line}): {}", self.message);
            }
            return write!(f, "runtime error ({file} - line {line}): {}", self.message);
        }

        writeln!(f, "runtime error: {}", self.message)?;
        writeln!(f, "call trace:",)?;
        for call in &self.call_stack {
            let function_name = if call.function_name == '\0' {
                "<global scope>".to_string()
            } else {
                format!("'{}'", call.function_name)
            };
            if call.file.is_empty() {
                writeln!(f, "=> {function_name} at line {}", call.line)?;
            } else {
                writeln!(
                    f,
                    "=> {function_name} ({}) at line {} ",
                    call.file, call.line,
                )?;
            }
        }
        Ok(())
    }
}

pub type ExecutionResult<T> = Result<T, ExecutionError>;

type NameMap<T> = [T; 26];

fn name_index(name: char) -> usize {
    (name as u8 - b'a') as usize
}

fn contains_quit(stmt: &StmtInstruction) -> bool {
    match stmt {
        StmtInstruction::Quit => true,
        StmtInstruction::If { body, .. } => body.iter().any(contains_quit),
        StmtInstruction::While { body, .. } => body.iter().any(contains_quit),
        _ => false,
    }
}

fn should_print(expr: &ExprInstruction) -> bool {
    // assignments should not be printed:
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/bc.html#tag_20_09_10
    !matches!(
        expr,
        ExprInstruction::Assignment { .. } | ExprInstruction::SetRegister { .. }
    )
}

fn get_or_extend(array: &mut Vec<Number>, index: usize) -> &mut Number {
    if index >= array.len() {
        array.resize_with(index + 1, Number::zero);
    }
    &mut array[index]
}

#[derive(Default)]
struct CallFrame {
    variables: NameMap<Option<Number>>,
    array_variables: NameMap<Option<Vec<Number>>>,
}

#[derive(Debug, PartialEq)]
enum ControlFlow {
    Return(Number),
    Quit,
    Break,
    None,
}

pub struct Interpreter {
    variables: NameMap<Number>,
    array_variables: NameMap<Vec<Number>>,
    functions: NameMap<Function>,
    call_frames: Vec<CallFrame>,
    scale: u64,
    ibase: u64,
    obase: u64,
    output: String,
    has_quit: bool,
    instruction_counter: usize,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            variables: Default::default(),
            array_variables: Default::default(),
            functions: Default::default(),
            call_frames: Vec::new(),
            scale: 0,
            ibase: 10,
            obase: 10,
            output: String::new(),
            has_quit: false,
            instruction_counter: 0,
        }
    }
}

impl Interpreter {
    fn take_and_clear_output(&mut self) -> String {
        let mut string = String::new();
        std::mem::swap(&mut self.output, &mut string);
        string
    }

    fn eval_named(&mut self, named: &NamedExpr) -> ExecutionResult<&mut Number> {
        match named {
            NamedExpr::VariableNumber(c) => {
                if let Some(call_frame) = self.call_frames.last_mut() {
                    if let Some(value) = &mut call_frame.variables[name_index(*c)] {
                        return Ok(value);
                    }
                }
                Ok(&mut self.variables[name_index(*c)])
            }
            NamedExpr::ArrayItem { name, index } => {
                let index = self
                    .eval_expr(index)?
                    .as_u64()
                    .ok_or("array index is too large")? as usize;
                if let Some(call_frame) = self.call_frames.last_mut() {
                    if let Some(array) = &mut call_frame.array_variables[name_index(*name)] {
                        return Ok(get_or_extend(array, index as usize));
                    }
                }
                Ok(get_or_extend(
                    &mut self.array_variables[name_index(*name)],
                    index,
                ))
            }
        }
    }

    fn call_function(&mut self, name: char, args: &[FunctionArgument]) -> ExecutionResult<Number> {
        let saved_instruction_counter = self.instruction_counter;
        let function = &self.functions[name_index(name)].clone();
        if function.name == '\0' {
            return Err("undefined function".into());
        }
        let mut call_frame = CallFrame::default();

        for (arg, param) in args.iter().zip(function.parameters.iter()) {
            // check if argument and parameter match
            match (arg, param) {
                (FunctionArgument::Expr(expr), Variable::Number(name)) => {
                    let value = self.eval_expr(expr)?;
                    call_frame.variables[name_index(*name)] = Some(value);
                }
                (FunctionArgument::ArrayVariable(arg_name), Variable::Array(param_name)) => {
                    // arrays are passed by value
                    let array = self.array_variables[name_index(*arg_name)].clone();
                    call_frame.array_variables[name_index(*param_name)] = Some(array)
                }
                _ => return Err("argument does not match parameter".into()),
            }
        }
        // set the instruction counter to 0 only after the arguments have been processed.
        // this way errors in the argument expression will be reported at the call site
        self.instruction_counter = 0;

        for local in function.locals.iter() {
            match local {
                Variable::Number(name) => {
                    call_frame.variables[name_index(*name)] = Some(0.into());
                }
                Variable::Array(name) => {
                    call_frame.array_variables[name_index(*name)] = Some(Vec::new());
                }
            }
        }
        let body = function.body.clone();

        self.call_frames.push(call_frame);
        for stmt in body.iter() {
            let evaluated_statement = self.eval_stmt(stmt).map_err(|e| {
                e.add_call(
                    function.name,
                    function.source_locations[self.instruction_counter],
                    function.file.clone(),
                )
            });
            match evaluated_statement {
                Err(e) => {
                    self.call_frames.pop();
                    self.instruction_counter = saved_instruction_counter;
                    return Err(e);
                }
                Ok(ControlFlow::Return(value)) => {
                    self.call_frames.pop();
                    self.instruction_counter = saved_instruction_counter;
                    return Ok(value);
                }
                Ok(ControlFlow::Break) | Ok(ControlFlow::Quit) => {
                    // both of these should never happen.
                    // A quit inside of a function definition
                    // should stop execution, and we can only call
                    // a function after its definition has been processed
                    // A break outside of a loop is a parser bug.
                    panic!("reached quit or break in function call")
                }
                _ => {}
            }
        }
        self.call_frames.pop();
        // from the POSIX standard:
        // > the value of the function shall be the value of the expression
        // > in the parentheses of the return statement or shall be zero
        // > if no expression is provided or if there is no return statement
        Ok(Number::zero())
    }

    fn eval_expr(&mut self, expr: &ExprInstruction) -> ExecutionResult<Number> {
        match expr {
            ExprInstruction::Number(x) => {
                Number::parse(x, self.ibase).ok_or("invalid digit for the current ibase".into())
            }
            ExprInstruction::GetRegister(reg) => match reg {
                Register::Scale => Ok(self.scale.into()),
                Register::IBase => Ok(self.ibase.into()),
                Register::OBase => Ok(self.obase.into()),
            },
            ExprInstruction::Named(named) => self.eval_named(named).cloned(),
            ExprInstruction::Builtin { function, arg } => match function {
                BuiltinFunction::Length => Ok(self.eval_expr(arg)?.length().into()),
                BuiltinFunction::Sqrt => self
                    .eval_expr(arg)?
                    .sqrt(self.scale)
                    .map_err(ExecutionError::from),
                BuiltinFunction::Scale => Ok(self.eval_expr(arg)?.scale().into()),
            },
            ExprInstruction::PreIncrement(named) => {
                let value = self.eval_named(named)?;
                value.inc();
                Ok(value.clone())
            }
            ExprInstruction::PreDecrement(named) => {
                let value = self.eval_named(named)?;
                value.dec();
                Ok(value.clone())
            }
            ExprInstruction::PostIncrement(named) => {
                let value = self.eval_named(named)?;
                let result = value.clone();
                value.inc();
                Ok(result)
            }
            ExprInstruction::PostDecrement(named) => {
                let value = self.eval_named(named)?;
                let result = value.clone();
                value.dec();
                Ok(result)
            }
            ExprInstruction::Call { name, args } => {
                let ic = self.instruction_counter;
                self.instruction_counter = 0;
                let call_result = self.call_function(*name, args);
                self.instruction_counter = ic;
                call_result
            }
            ExprInstruction::Assignment { named, value } => {
                let value = self.eval_expr(value)?;
                self.eval_named(named)?.clone_from(&value);
                Ok(value)
            }
            ExprInstruction::SetRegister { register, value } => {
                // if the value is a single digit it has to be interpreted
                // as an hexadecimal number, regardless of the value of ibase
                let value = match value.as_ref() {
                    ExprInstruction::Number(n) if n.len() == 1 => {
                        // this cannot fail because the parser ensures that
                        // the value is a valid hexadecimal number
                        Number::parse(n, 16).unwrap()
                    }
                    _ => self.eval_expr(value)?,
                };

                match register {
                    Register::Scale => {
                        self.scale = value
                            .as_u64()
                            .ok_or("the value assigned to scale is too large")?
                    }
                    Register::IBase => {
                        if let Some(new_ibase) = value.as_u64() {
                            if (2..=16).contains(&new_ibase) {
                                self.ibase = new_ibase;
                                return Ok(value);
                            }
                        }
                        return Err("ibase must be between 2 and 16".into());
                    }
                    Register::OBase => {
                        if let Some(new_obase) = value.as_u64() {
                            if new_obase >= 2 {
                                self.obase = new_obase;
                            } else {
                                return Err("obase must be greater than 1".into());
                            }
                        } else {
                            return Err("value assigned to obase is too large".into());
                        }
                    }
                }
                Ok(value)
            }
            ExprInstruction::UnaryMinus(expr) => Ok(self.eval_expr(expr)?.negate()),
            ExprInstruction::Add(lhs, rhs) => Ok(self.eval_expr(lhs)?.add(&self.eval_expr(rhs)?)),
            ExprInstruction::Sub(lhs, rhs) => Ok(self.eval_expr(lhs)?.sub(&self.eval_expr(rhs)?)),
            ExprInstruction::Mul(lhs, rhs) => {
                Ok(self.eval_expr(lhs)?.mul(&self.eval_expr(rhs)?, self.scale))
            }
            ExprInstruction::Div(lhs, rhs) => Ok(self
                .eval_expr(lhs)?
                .div(&self.eval_expr(rhs)?, self.scale)?),
            ExprInstruction::Mod(lhs, rhs) => self
                .eval_expr(lhs)?
                .modulus(&self.eval_expr(rhs)?, self.scale)
                .map_err(ExecutionError::from),
            ExprInstruction::Pow(lhs, rhs) => self
                .eval_expr(lhs)?
                .pow(&self.eval_expr(rhs)?, self.scale)
                .map_err(ExecutionError::from),
        }
    }

    fn eval_condition(&mut self, condition: &ConditionInstruction) -> ExecutionResult<bool> {
        match condition {
            ConditionInstruction::Expr(expr) => self.eval_expr(expr).map(|val| !val.is_zero()),
            ConditionInstruction::Eq(lhs, rhs) => Ok(self.eval_expr(lhs)? == self.eval_expr(rhs)?),
            ConditionInstruction::Ne(lhs, rhs) => Ok(self.eval_expr(lhs)? != self.eval_expr(rhs)?),
            ConditionInstruction::Lt(lhs, rhs) => Ok(self.eval_expr(lhs)? < self.eval_expr(rhs)?),
            ConditionInstruction::Gt(lhs, rhs) => Ok(self.eval_expr(lhs)? > self.eval_expr(rhs)?),
            ConditionInstruction::Leq(lhs, rhs) => Ok(self.eval_expr(lhs)? <= self.eval_expr(rhs)?),
            ConditionInstruction::Geq(lhs, rhs) => Ok(self.eval_expr(lhs)? >= self.eval_expr(rhs)?),
        }
    }

    fn eval_stmt(&mut self, stmt: &StmtInstruction) -> ExecutionResult<ControlFlow> {
        let instruction_counter_start = self.instruction_counter;
        let mut stmt_instruction_count = 1;
        match stmt {
            StmtInstruction::Break => {
                return Ok(ControlFlow::Break);
            }
            StmtInstruction::Quit => {
                return Ok(ControlFlow::Quit);
            }
            StmtInstruction::Return => {
                return Ok(ControlFlow::Return(Number::zero()));
            }
            StmtInstruction::ReturnExpr(expr) => {
                let value = self.eval_expr(expr)?;
                return Ok(ControlFlow::Return(value));
            }
            StmtInstruction::If {
                condition,
                instruction_count,
                body,
            } => {
                stmt_instruction_count = *instruction_count + 1;
                if self.eval_condition(condition)? {
                    // count the condition
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt)?;
                        // any control flow instruction in the body of the
                        // if needs to be handled by the caller
                        if control_flow != ControlFlow::None {
                            return Ok(control_flow);
                        }
                    }
                }
            }
            StmtInstruction::While {
                condition,
                instruction_count,
                body,
            } => {
                stmt_instruction_count = *instruction_count + 1;
                'while_loop: while self.eval_condition(condition)? {
                    // count the condition
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt)?;
                        if control_flow == ControlFlow::Break {
                            break 'while_loop;
                        }
                        if control_flow != ControlFlow::None {
                            // we either hit a return or quit
                            // so we need to pass that up the stack
                            return Ok(control_flow);
                        }
                    }
                    // reset the instruction counter to the start of the loop
                    self.instruction_counter = instruction_counter_start;
                }
            }
            StmtInstruction::For {
                init,
                condition,
                update,
                instruction_count,
                body,
            } => {
                stmt_instruction_count = *instruction_count + 1;
                self.eval_expr(init)?;
                'for_loop: while self.eval_condition(condition)? {
                    // count init condition and update
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt)?;
                        if control_flow == ControlFlow::Break {
                            break 'for_loop;
                        }
                        if control_flow != ControlFlow::None {
                            return Ok(control_flow);
                        }
                    }
                    // reset the instruction counter to the start of the loop
                    self.instruction_counter = instruction_counter_start;
                    self.eval_expr(update)?;
                }
            }
            StmtInstruction::String(s) => self.output.push_str(s),
            StmtInstruction::Expr(expr) => {
                let value = self.eval_expr(expr)?;
                if should_print(expr) {
                    // this should never fail
                    writeln!(&mut self.output, "{}", value.to_string(self.obase))
                        .expect("error appending to string");
                }
            }
            StmtInstruction::DefineFunction { .. } => {
                // the language grammar ensures that this is never reached
                panic!("function definition outside of the global scope")
            }
        }
        self.instruction_counter = instruction_counter_start + stmt_instruction_count;
        Ok(ControlFlow::None)
    }

    pub fn exec(&mut self, program: Program) -> ExecutionResult<String> {
        self.instruction_counter = 0;
        for stmt in program.instructions {
            if let StmtInstruction::DefineFunction { name, function } = stmt {
                // we handle this here because we need to store the function.
                // doing it in eval_stmt would not work because we would need
                // to clone from the reference. Since functions can only be
                // defined in the global scope, this is valid.

                // first we need to check if the definition contains quit,
                // in which case we need to stop execution
                if function.body.iter().any(contains_quit) {
                    self.has_quit = true;
                    return Ok(self.take_and_clear_output());
                }

                self.functions[name_index(name)] = function;
            } else {
                let control_flow = self.eval_stmt(&stmt).map_err(|mut e| {
                    e.partial_output = self.take_and_clear_output();
                    e.global_source(
                        program.source_locations[self.instruction_counter],
                        program.file.clone(),
                    )
                })?;
                match control_flow {
                    // both of these should have been handled earlier
                    // by the parser
                    ControlFlow::Return(_) => {
                        panic!("return outside of function");
                    }
                    ControlFlow::Break => {
                        panic!("break outside of loop");
                    }
                    _ => {}
                }
                // we can't trust the return value of eval_stmt because
                // unexecuted branches will not return ControlFlow::Quit,
                // but we need still need to stop execution
                if contains_quit(&stmt) {
                    self.has_quit = true;
                    return Ok(self.take_and_clear_output());
                }
            }
        }
        Ok(self.take_and_clear_output())
    }

    pub fn has_quit(&self) -> bool {
        self.has_quit
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_number() {
        let mut interpreter = Interpreter::default();
        // ```
        // 5
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Number(
                    "5".to_string(),
                ))]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn print_uninitialized_variable() {
        let mut interpreter = Interpreter::default();
        // ```
        // a
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Named(
                    NamedExpr::VariableNumber('a'),
                ))]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n");
    }

    #[test]
    fn test_call_builtin_scale() {
        let mut interpreter = Interpreter::default();
        // ```
        // scale(5)
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Builtin {
                    function: BuiltinFunction::Scale,
                    arg: Box::new(ExprInstruction::Number("5".to_string())),
                })]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n");
    }

    #[test]
    fn test_call_builtin_sqrt() {
        let mut interpreter = Interpreter::default();
        // ```
        // sqrt(25)
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Builtin {
                    function: BuiltinFunction::Sqrt,
                    arg: Box::new(ExprInstruction::Number("25".to_string())),
                })]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_call_builtin_legth() {
        let mut interpreter = Interpreter::default();
        // ```
        // length(5)
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Builtin {
                    function: BuiltinFunction::Length,
                    arg: Box::new(ExprInstruction::Number("5".to_string())),
                })]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "1\n");
    }

    #[test]
    fn test_preincrement() {
        let mut interpreter = Interpreter::default();
        // ```
        // ++a
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::PreIncrement(
                        NamedExpr::VariableNumber('a'),
                    )),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "1\n1\n");
    }

    #[test]
    fn test_predecrement() {
        let mut interpreter = Interpreter::default();
        // ```
        // --a
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::PreDecrement(
                        NamedExpr::VariableNumber('a'),
                    )),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "-1\n-1\n");
    }

    #[test]
    fn test_postincrement() {
        let mut interpreter = Interpreter::default();
        // ```
        // a++
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::PostIncrement(
                        NamedExpr::VariableNumber('a'),
                    )),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n1\n");
    }

    #[test]
    fn test_postdecrement() {
        let mut interpreter = Interpreter::default();
        // ```
        // a--
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::PostDecrement(
                        NamedExpr::VariableNumber('a'),
                    )),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n-1\n");
    }

    #[test]
    fn test_function_call() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        // 5
        // }
        // f()
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            body: [StmtInstruction::Expr(ExprInstruction::Number(
                                "5".to_string(),
                            ))]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![],
                    }),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n0\n");
    }

    #[test]
    fn test_assignment() {
        let mut interpreter = Interpreter::default();
        // ```
        // a = 5
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::VariableNumber('a'),
                        value: Box::new(ExprInstruction::Number("5".to_string())),
                    }),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_quit() {
        let mut interpreter = Interpreter::default();
        // ```
        // quit
        // ```
        let output = interpreter
            .exec(vec![StmtInstruction::Quit].into())
            .unwrap();
        assert_eq!(output, "");
        assert!(interpreter.has_quit());
    }

    #[test]
    fn test_break_out_of_loop() {
        let mut interpreter = Interpreter::default();
        // ```
        // while (1) { break; 1 }
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::While {
                    condition: ConditionInstruction::Expr(ExprInstruction::Number("1".to_string())),
                    instruction_count: 2,
                    body: vec![
                        StmtInstruction::Break,
                        StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                    ],
                }]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
    }

    #[test]
    fn test_call_function_without_return() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        // }
        // f()
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![],
                    }),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n");
    }

    #[test]
    fn test_call_function_with_return_expression() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        //   return(5)
        // }
        // f()
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            body: [StmtInstruction::ReturnExpr(ExprInstruction::Number(
                                "5".to_string(),
                            ))]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![],
                    }),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_if_true_branch() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (1) {
        //   5
        // }
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::If {
                    condition: ConditionInstruction::Expr(ExprInstruction::Number("1".to_string())),
                    instruction_count: 1,
                    body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                        "5".to_string(),
                    ))],
                }]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_if_false_branch() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (0) {
        //   5
        // }
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::If {
                    condition: ConditionInstruction::Expr(ExprInstruction::Number("0".to_string())),
                    instruction_count: 1,
                    body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                        "5".to_string(),
                    ))],
                }]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
    }

    #[test]
    fn test_assignment_does_not_print() {
        let mut interpreter = Interpreter::default();
        // ```
        // a = 5
        // ```
        let output = interpreter
            .exec(
                vec![StmtInstruction::Expr(ExprInstruction::Assignment {
                    named: NamedExpr::VariableNumber('a'),
                    value: Box::new(ExprInstruction::Number("5".to_string())),
                })]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
    }

    #[test]
    fn test_assign_to_array() {
        let mut interpreter = Interpreter::default();
        // ```
        // a[0] = 5
        // a[0]
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::ArrayItem {
                            name: 'a',
                            index: Box::new(ExprInstruction::Number("0".to_string())),
                        },
                        value: Box::new(ExprInstruction::Number("5".to_string())),
                    }),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::ArrayItem {
                        name: 'a',
                        index: Box::new(ExprInstruction::Number("0".to_string())),
                    })),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_exit_after_quit_in_function_definition() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        //   quit
        // }
        // 1
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            body: [StmtInstruction::Quit].into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
        assert!(interpreter.has_quit());
    }

    #[test]
    fn test_exit_after_quit_in_unexecuted_if() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (0) {
        //   1
        //   quit
        // }
        // 1
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::If {
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "0".to_string(),
                        )),
                        instruction_count: 2,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                            StmtInstruction::Quit,
                        ],
                    },
                    StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
        assert!(interpreter.has_quit());
    }

    #[test]
    fn test_exit_after_quit_in_unexecuted_while() {
        let mut interpreter = Interpreter::default();
        // ```
        // while (0) {
        //   2
        //   quit
        // }
        // 1
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::While {
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "0".to_string(),
                        )),
                        instruction_count: 2,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("2".to_string())),
                            StmtInstruction::Quit,
                        ],
                    },
                    StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "");
        assert!(interpreter.has_quit());
    }

    #[test]
    fn test_assign_to_function_local_does_not_change_global() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        //   auto a;
        //   a = 5
        // }
        // f()
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            locals: [Variable::Number('a')].into(),
                            body: [StmtInstruction::Expr(ExprInstruction::Assignment {
                                named: NamedExpr::VariableNumber('a'),
                                value: Box::new(ExprInstruction::Number("5".to_string())),
                            })]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![],
                    }),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n0\n");
    }

    #[test]
    fn test_assign_to_function_parameter_does_not_change_global() {
        let mut interpreter = Interpreter::default();
        // ```
        // f(a) {
        //   a = 5
        // }
        // a = 1
        // f(a)
        // a
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            parameters: [Variable::Number('a')].into(),
                            body: [StmtInstruction::Expr(ExprInstruction::Assignment {
                                named: NamedExpr::VariableNumber('a'),
                                value: Box::new(ExprInstruction::Number("5".to_string())),
                            })]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::VariableNumber('a'),
                        value: Box::new(ExprInstruction::Number("1".to_string())),
                    }),
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![FunctionArgument::Expr(ExprInstruction::Named(
                            NamedExpr::VariableNumber('a'),
                        ))],
                    }),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "0\n1\n");
    }

    #[test]
    fn test_standard_parameter_passing() {
        let mut interpreter = Interpreter::default();
        // ```
        // define f(a) {
        //   return(a)
        // }
        // f(5)
        //```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            parameters: [Variable::Number('a')].into(),
                            body: [StmtInstruction::ReturnExpr(ExprInstruction::Named(
                                NamedExpr::VariableNumber('a'),
                            ))]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![FunctionArgument::Expr(ExprInstruction::Number(
                            "5".to_string(),
                        ))],
                    }),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "5\n");
    }

    #[test]
    fn test_pass_arrays_by_value() {
        let mut interpreter = Interpreter::default();
        // ```
        // define f(a) {
        //   a[0]
        //   a[0] = 5
        // }
        // a[0] = 1
        // f(a)
        // a[0]
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            parameters: [Variable::Array('a')].into(),
                            body: [
                                StmtInstruction::Expr(ExprInstruction::Named(
                                    NamedExpr::ArrayItem {
                                        name: 'a',
                                        index: Box::new(ExprInstruction::Number("0".to_string())),
                                    },
                                )),
                                StmtInstruction::Expr(ExprInstruction::Assignment {
                                    named: NamedExpr::ArrayItem {
                                        name: 'a',
                                        index: Box::new(ExprInstruction::Number("0".to_string())),
                                    },
                                    value: Box::new(ExprInstruction::Number("5".to_string())),
                                }),
                            ]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::ArrayItem {
                            name: 'a',
                            index: Box::new(ExprInstruction::Number("0".to_string())),
                        },
                        value: Box::new(ExprInstruction::Number("1".to_string())),
                    }),
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![FunctionArgument::ArrayVariable('a')],
                    }),
                    StmtInstruction::Expr(ExprInstruction::Named(NamedExpr::ArrayItem {
                        name: 'a',
                        index: Box::new(ExprInstruction::Number("0".to_string())),
                    })),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "1\n0\n1\n");
    }

    #[test]
    fn test_assignment_of_a_single_value_to_base_register_is_hexadecimal() {
        let mut interpreter = Interpreter::default();
        // ```
        // obase = F
        // obase
        // ```
        let output = interpreter
            .exec(
                vec![
                    StmtInstruction::Expr(ExprInstruction::SetRegister {
                        register: Register::OBase,
                        value: Box::new(ExprInstruction::Number("F".to_string())),
                    }),
                    StmtInstruction::Expr(ExprInstruction::GetRegister(Register::OBase)),
                ]
                .into(),
            )
            .unwrap();
        assert_eq!(output, "10\n");
    }

    #[test]
    fn test_call_undefined_function_is_error() {
        let mut interpreter = Interpreter::default();
        // ```
        // f()
        // ```
        let output = interpreter.exec(Program {
            instructions: vec![StmtInstruction::Expr(ExprInstruction::Call {
                name: 'f',
                args: vec![],
            })],
            source_locations: vec![1],
            file: "".into(),
        });
        assert!(output.is_err());
    }

    #[test]
    fn test_error_inside_while_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // i = 1
        // while (i > - 1) {
        //   1
        //   1 / i
        //   --i
        // }
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::VariableNumber('i'),
                        value: Box::new(ExprInstruction::Number("1".to_string())),
                    }),
                    StmtInstruction::While {
                        condition: ConditionInstruction::Gt(
                            ExprInstruction::Named(NamedExpr::VariableNumber('i')),
                            ExprInstruction::UnaryMinus(Box::new(ExprInstruction::Number(
                                "1".to_string(),
                            ))),
                        ),
                        instruction_count: 3,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Div(
                                Box::new(ExprInstruction::Number("1".to_string())),
                                Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('i'))),
                            )),
                            StmtInstruction::Expr(ExprInstruction::PreDecrement(
                                NamedExpr::VariableNumber('i'),
                            )),
                        ],
                    },
                ],
                source_locations: vec![1, 2, 3, 4, 5],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 4);
    }

    #[test]
    fn test_error_after_executed_while_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // i = 0
        // while (i < 10) {
        //   ++i
        // }
        // 1 ^ 2.2
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::Expr(ExprInstruction::Assignment {
                        named: NamedExpr::VariableNumber('i'),
                        value: Box::new(ExprInstruction::Number("0".to_string())),
                    }),
                    StmtInstruction::While {
                        condition: ConditionInstruction::Lt(
                            ExprInstruction::Named(NamedExpr::VariableNumber('i')),
                            ExprInstruction::Number("10".to_string()),
                        ),
                        instruction_count: 1,
                        body: vec![StmtInstruction::Expr(ExprInstruction::PreIncrement(
                            NamedExpr::VariableNumber('i'),
                        ))],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 3, 4],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 4);
    }

    #[test]
    fn test_err_after_unexecuted_while_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // while(0) {
        //   1
        //   2
        //   3
        //   4
        // }
        // 1 ^ 2.2
        //```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::While {
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "0".to_string(),
                        )),
                        instruction_count: 4,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("2".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("3".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("4".to_string())),
                        ],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 3, 4, 5, 6],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 6);
    }

    #[test]
    fn test_error_inside_for_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // for (a = 0; a > -1; --a) {
        //   1
        //   1 / a
        //   2
        // }
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![StmtInstruction::For {
                    init: ExprInstruction::Assignment {
                        named: NamedExpr::VariableNumber('a'),
                        value: Box::new(ExprInstruction::Number("0".to_string())),
                    },
                    condition: ConditionInstruction::Gt(
                        ExprInstruction::Named(NamedExpr::VariableNumber('a')),
                        ExprInstruction::UnaryMinus(Box::new(ExprInstruction::Number(
                            "1".to_string(),
                        ))),
                    ),
                    update: ExprInstruction::PreDecrement(NamedExpr::VariableNumber('a')),
                    instruction_count: 3,
                    body: vec![
                        StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                        StmtInstruction::Expr(ExprInstruction::Div(
                            Box::new(ExprInstruction::Number("1".to_string())),
                            Box::new(ExprInstruction::Named(NamedExpr::VariableNumber('a'))),
                        )),
                        StmtInstruction::Expr(ExprInstruction::Number("2".to_string())),
                    ],
                }],
                source_locations: vec![1, 2, 3, 4, 5],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 3);
    }

    #[test]
    fn test_error_after_executed_for_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // for (a = 0; a < 5; a++) {
        //   1
        // }
        // 1 ^ 2.2
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::For {
                        init: ExprInstruction::Assignment {
                            named: NamedExpr::VariableNumber('a'),
                            value: Box::new(ExprInstruction::Number("0".to_string())),
                        },
                        condition: ConditionInstruction::Lt(
                            ExprInstruction::Named(NamedExpr::VariableNumber('a')),
                            ExprInstruction::Number("5".to_string()),
                        ),
                        update: ExprInstruction::PostIncrement(NamedExpr::VariableNumber('a')),
                        instruction_count: 1,
                        body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                            "1".to_string(),
                        ))],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 4],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 4);
    }

    #[test]
    fn test_error_after_unexecuted_for_loop_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // for (a = 0; 0; a++) {
        //   1
        //   2
        //   3
        //   4
        // }
        // 1 ^ 2.2
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::For {
                        init: ExprInstruction::Assignment {
                            named: NamedExpr::VariableNumber('a'),
                            value: Box::new(ExprInstruction::Number("0".to_string())),
                        },
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "0".to_string(),
                        )),
                        update: ExprInstruction::PostIncrement(NamedExpr::VariableNumber('a')),
                        instruction_count: 4,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("2".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("3".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("4".to_string())),
                        ],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 3, 4, 5, 7],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 7);
    }

    #[test]
    fn test_error_inside_if_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (1) {
        //   1
        //   1 / 0
        // }
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![StmtInstruction::If {
                    condition: ConditionInstruction::Expr(ExprInstruction::Number("1".to_string())),
                    instruction_count: 2,
                    body: vec![
                        StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                        StmtInstruction::Expr(ExprInstruction::Div(
                            Box::new(ExprInstruction::Number("1".to_string())),
                            Box::new(ExprInstruction::Number("0".to_string())),
                        )),
                    ],
                }],
                source_locations: vec![1, 2, 3],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 3);
    }

    #[test]
    fn test_error_after_executed_if_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (1) {
        //   1
        // }
        // 1 ^ 2.2
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::If {
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "1".to_string(),
                        )),
                        instruction_count: 1,
                        body: vec![StmtInstruction::Expr(ExprInstruction::Number(
                            "1".to_string(),
                        ))],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 3],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 3);
    }

    #[test]
    fn test_error_after_unexecuted_if_reports_correct_line() {
        let mut interpreter = Interpreter::default();
        // ```
        // if (0) {
        //   1
        //   2
        //   3
        //   4
        // }
        // 1 ^ 2.2
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::If {
                        condition: ConditionInstruction::Expr(ExprInstruction::Number(
                            "0".to_string(),
                        )),
                        instruction_count: 4,
                        body: vec![
                            StmtInstruction::Expr(ExprInstruction::Number("1".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("2".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("3".to_string())),
                            StmtInstruction::Expr(ExprInstruction::Number("4".to_string())),
                        ],
                    },
                    StmtInstruction::Expr(ExprInstruction::Pow(
                        Box::new(ExprInstruction::Number("1".to_string())),
                        Box::new(ExprInstruction::Number("2.2".to_string())),
                    )),
                ],
                source_locations: vec![1, 2, 3, 4, 5, 7],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 7);
    }

    #[test]
    fn test_function_call_errors_report_correct_lines() {
        let mut interpreter = Interpreter::default();
        // ```
        // f() {
        //   1 ^ 2.2
        // }
        // f()
        // ```
        let err = interpreter
            .exec(Program {
                instructions: vec![
                    StmtInstruction::DefineFunction {
                        name: 'f',
                        function: Function {
                            name: 'f',
                            source_locations: [2].into(),
                            body: [StmtInstruction::Expr(ExprInstruction::Pow(
                                Box::new(ExprInstruction::Number("1".to_string())),
                                Box::new(ExprInstruction::Number("2.2".to_string())),
                            ))]
                            .into(),
                            ..Default::default()
                        },
                    },
                    StmtInstruction::Expr(ExprInstruction::Call {
                        name: 'f',
                        args: vec![],
                    }),
                ],
                source_locations: vec![4],
                file: "".into(),
            })
            .expect_err("expected error");
        assert_eq!(err.call_stack[0].line, 2);
        assert_eq!(err.call_stack[1].line, 4);
    }
}
