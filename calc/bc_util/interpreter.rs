//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{collections::HashMap, io, rc::Rc};

use crate::bc_util::instructions::Variable;

use super::{
    instructions::{
        BuiltinFunction, ConditionInstruction, ExprInstruction, Function, FunctionArgument,
        NamedExpr, Program, Register, StmtInstruction,
    },
    number::Number,
    output::OutputWriter,
};

#[derive(Debug)]
struct ErrorCall {
    function_name: char,
    line: usize,
    file: Rc<str>,
}

#[derive(Debug)]
pub struct ExecutionError {
    message: String,
    call_stack: Vec<ErrorCall>,
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
}

impl From<&'static str> for ExecutionError {
    fn from(message: &'static str) -> Self {
        Self {
            message: message.to_string(),
            call_stack: Vec::new(),
        }
    }
}

impl From<io::Error> for ExecutionError {
    fn from(e: io::Error) -> Self {
        Self {
            message: format!("cannot write output: {e}"),
            call_stack: Vec::new(),
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

// POSIX limit maxima. POSIX defines these as minimum-maxima; we use GNU bc's
// generous values so real programs are not rejected while pathological inputs
// can no longer drive unbounded allocation.
const BC_SCALE_MAX: u64 = i32::MAX as u64; // matches GNU bc (2147483647)
const BC_BASE_MAX: u64 = i32::MAX as u64; // obase upper bound, matches GNU bc
const BC_DIM_MAX: u64 = 16_777_216; // array elements, matches GNU bc (2^24)

/// Maximum depth of nested evaluation.
///
/// Expression evaluation and function calls recurse on the machine stack, so
/// without a limit a runaway bc recursion or a very long operator chain aborts
/// the process on a guard page instead of reporting an error. `bc.rs` runs the
/// interpreter on a stack sized so that this limit, and not the stack, is what
/// stops it. Unoptimized builds use far larger frames than release ones, so the
/// value is chosen to hold for both. A call costs a few levels, which leaves
/// room for bc recursion some thousands deep -- orders of magnitude beyond what
/// real programs use.
const MAX_EVAL_DEPTH: usize = 50_000;

type NameMap<T> = [T; 26];

/// A bc array.
///
/// Indices run to `BC_DIM_MAX`, and sparse use is a normal idiom, so elements
/// are stored only where they have been set: `a[16777214] = 1` should cost one
/// entry rather than sixteen million.
type Array = HashMap<usize, Number>;

fn name_index(name: char) -> usize {
    (name as u8 - b'a') as usize
}

fn contains_quit(stmt: &StmtInstruction) -> bool {
    match stmt {
        StmtInstruction::Quit => true,
        StmtInstruction::If { body, .. } => body.iter().any(contains_quit),
        StmtInstruction::While { body, .. } => body.iter().any(contains_quit),
        StmtInstruction::For { body, .. } => body.iter().any(contains_quit),
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

/// The global values a call displaced, to be put back when it returns.
///
/// POSIX (XCU bc, "on entry to a function, the old values of the names that
/// appear as parameters and as automatic variables shall be pushed onto a
/// stack ... references to any of these names from other functions that are
/// called from this function also refer to the new value") describes dynamic
/// scoping. Saving the old value and writing the new one into the global slot
/// gives exactly that, and leaves name lookup with nothing to search.
#[derive(Default)]
struct CallFrame {
    variables: Vec<(usize, Number)>,
    arrays: Vec<(usize, Array)>,
}

/// A parameter's value, computed in the caller's scope before any binding is
/// installed.
enum Binding {
    Number(usize, Number),
    Array(usize, Array),
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
    array_variables: NameMap<Array>,
    functions: NameMap<Function>,
    call_frames: Vec<CallFrame>,
    scale: u64,
    ibase: u64,
    obase: u64,
    has_quit: bool,
    instruction_counter: usize,
    depth: usize,
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
            has_quit: false,
            instruction_counter: 0,
            depth: 0,
        }
    }
}

impl Interpreter {
    /// Evaluate an array subscript, checked against `BC_DIM_MAX`.
    fn array_index(
        &mut self,
        index: &ExprInstruction,
        out: &mut OutputWriter,
    ) -> ExecutionResult<usize> {
        let value = self.eval_expr(index, out)?;
        if value.is_negative() {
            return Err("array index cannot be negative".into());
        }
        let index = value.as_u64().ok_or("array index is too large")?;
        if index >= BC_DIM_MAX {
            return Err("array index out of bounds".into());
        }
        Ok(index as usize)
    }

    /// Read a named expression. An element that was never assigned reads as
    /// zero, without being created.
    fn read_named(&mut self, named: &NamedExpr, out: &mut OutputWriter) -> ExecutionResult<Number> {
        match named {
            NamedExpr::VariableNumber(c) => Ok(self.variables[name_index(*c)].clone()),
            NamedExpr::ArrayItem { name, index } => {
                let index = self.array_index(index, out)?;
                Ok(self.array_variables[name_index(*name)]
                    .get(&index)
                    .cloned()
                    .unwrap_or_else(Number::zero))
            }
        }
    }

    /// Borrow a named expression for assignment, creating the element if it
    /// does not exist yet.
    fn write_named(
        &mut self,
        named: &NamedExpr,
        out: &mut OutputWriter,
    ) -> ExecutionResult<&mut Number> {
        match named {
            NamedExpr::VariableNumber(c) => Ok(&mut self.variables[name_index(*c)]),
            NamedExpr::ArrayItem { name, index } => {
                let index = self.array_index(index, out)?;
                Ok(self.array_variables[name_index(*name)]
                    .entry(index)
                    .or_insert_with(Number::zero))
            }
        }
    }

    /// Put back the globals a call displaced.
    fn pop_call_frame(&mut self) {
        let Some(frame) = self.call_frames.pop() else {
            return;
        };
        // Restore in reverse: a name used both as a parameter and as an auto
        // was saved twice, and only the first save holds the caller's value.
        for (slot, value) in frame.variables.into_iter().rev() {
            self.variables[slot] = value;
        }
        for (slot, array) in frame.arrays.into_iter().rev() {
            self.array_variables[slot] = array;
        }
    }

    fn call_function(
        &mut self,
        name: char,
        args: &[FunctionArgument],
        out: &mut OutputWriter,
    ) -> ExecutionResult<Number> {
        self.depth += 1;
        if self.depth > MAX_EVAL_DEPTH {
            self.depth -= 1;
            return Err("evaluation nested too deeply".into());
        }
        let result = self.call_function_inner(name, args, out);
        self.depth -= 1;
        result
    }

    fn call_function_inner(
        &mut self,
        name: char,
        args: &[FunctionArgument],
        out: &mut OutputWriter,
    ) -> ExecutionResult<Number> {
        let saved_instruction_counter = self.instruction_counter;
        let function = &self.functions[name_index(name)].clone();
        if function.name == '\0' {
            return Err("undefined function".into());
        }
        if args.len() != function.parameters.len() {
            return Err("wrong number of arguments".into());
        }

        // Evaluate every argument before binding any of them: the argument
        // expressions belong to the caller's scope, and a parameter's name may
        // be one a later argument reads.
        let mut bindings = Vec::with_capacity(args.len());
        for (arg, param) in args.iter().zip(function.parameters.iter()) {
            match (arg, param) {
                (FunctionArgument::Expr(expr), Variable::Number(name)) => {
                    let value = self.eval_expr(expr, out)?;
                    bindings.push(Binding::Number(name_index(*name), value));
                }
                (FunctionArgument::ArrayVariable(arg_name), Variable::Array(param_name)) => {
                    // Arrays are passed by value, taken from whatever the name
                    // is bound to now -- which may be a caller's local.
                    let array = self.array_variables[name_index(*arg_name)].clone();
                    bindings.push(Binding::Array(name_index(*param_name), array));
                }
                _ => return Err("argument does not match parameter".into()),
            }
        }
        // set the instruction counter to 0 only after the arguments have been processed.
        // this way errors in the argument expression will be reported at the call site
        self.instruction_counter = 0;

        let mut call_frame = CallFrame::default();
        for binding in bindings {
            match binding {
                Binding::Number(slot, value) => {
                    let old = std::mem::replace(&mut self.variables[slot], value);
                    call_frame.variables.push((slot, old));
                }
                Binding::Array(slot, array) => {
                    let old = std::mem::replace(&mut self.array_variables[slot], array);
                    call_frame.arrays.push((slot, old));
                }
            }
        }
        for local in function.locals.iter() {
            match local {
                Variable::Number(name) => {
                    let slot = name_index(*name);
                    let old = std::mem::take(&mut self.variables[slot]);
                    call_frame.variables.push((slot, old));
                }
                Variable::Array(name) => {
                    let slot = name_index(*name);
                    let old = std::mem::take(&mut self.array_variables[slot]);
                    call_frame.arrays.push((slot, old));
                }
            }
        }
        let body = function.body.clone();

        self.call_frames.push(call_frame);
        for stmt in body.iter() {
            let evaluated_statement = self.eval_stmt(stmt, out).map_err(|e| {
                e.add_call(
                    function.name,
                    function.source_locations[self.instruction_counter],
                    function.file.clone(),
                )
            });
            match evaluated_statement {
                Err(e) => {
                    self.pop_call_frame();
                    self.instruction_counter = saved_instruction_counter;
                    return Err(e);
                }
                Ok(ControlFlow::Return(value)) => {
                    self.pop_call_frame();
                    self.instruction_counter = saved_instruction_counter;
                    return Ok(value);
                }
                Ok(ControlFlow::Quit) => {
                    // A quit reached at runtime inside a function body. The
                    // static contains_quit check normally stops execution when
                    // the definition is read, so this is defensive: stop
                    // gracefully rather than crashing.
                    self.has_quit = true;
                    self.pop_call_frame();
                    self.instruction_counter = saved_instruction_counter;
                    return Ok(Number::zero());
                }
                Ok(ControlFlow::Break) => {
                    // A break not bound to a loop ends the function harmlessly.
                    self.pop_call_frame();
                    self.instruction_counter = saved_instruction_counter;
                    return Ok(Number::zero());
                }
                _ => {}
            }
        }
        self.pop_call_frame();
        // from the POSIX standard:
        // > the value of the function shall be the value of the expression
        // > in the parentheses of the return statement or shall be zero
        // > if no expression is provided or if there is no return statement
        Ok(Number::zero())
    }

    fn register_value(&self, register: Register) -> u64 {
        match register {
            Register::Scale => self.scale,
            Register::IBase => self.ibase,
            Register::OBase => self.obase,
        }
    }

    /// Assign to a register, enforcing the bounds POSIX places on each.
    fn set_register(&mut self, register: Register, value: &Number) -> ExecutionResult<()> {
        // A value that does not fit in a u64 is reported as too large; a
        // negative one is out of range in the other direction, and saying
        // "too large" about it is simply wrong.
        if value.is_negative() {
            return Err(match register {
                Register::Scale => "scale cannot be negative",
                Register::IBase => "ibase must be between 2 and 16",
                Register::OBase => "obase must be greater than 1",
            }
            .into());
        }
        match register {
            Register::Scale => {
                let new_scale = value
                    .as_u64()
                    .ok_or("the value assigned to scale is too large")?;
                if new_scale > BC_SCALE_MAX {
                    return Err("scale is too large".into());
                }
                self.scale = new_scale;
            }
            Register::IBase => {
                let new_ibase = value
                    .as_u64()
                    .filter(|base| (2..=16).contains(base))
                    .ok_or("ibase must be between 2 and 16")?;
                self.ibase = new_ibase;
            }
            Register::OBase => {
                let new_obase = value
                    .as_u64()
                    .ok_or("value assigned to obase is too large")?;
                if new_obase < 2 {
                    return Err("obase must be greater than 1".into());
                }
                if new_obase > BC_BASE_MAX {
                    return Err("obase is too large".into());
                }
                self.obase = new_obase;
            }
        }
        Ok(())
    }

    /// `register++` and `register--`, yielding the new value for the prefix
    /// form and the old one for the postfix form.
    fn step_register(
        &mut self,
        register: Register,
        by: i64,
        prefix: bool,
    ) -> ExecutionResult<Number> {
        let old = self.register_value(register);
        let mut new = Number::from(old);
        if by >= 0 {
            new.inc();
        } else {
            new.dec();
        }
        self.set_register(register, &new)?;
        Ok(if prefix { new } else { Number::from(old) })
    }

    fn eval_expr(
        &mut self,
        expr: &ExprInstruction,
        out: &mut OutputWriter,
    ) -> ExecutionResult<Number> {
        self.depth += 1;
        if self.depth > MAX_EVAL_DEPTH {
            self.depth -= 1;
            return Err("evaluation nested too deeply".into());
        }
        let result = self.eval_expr_inner(expr, out);
        self.depth -= 1;
        result
    }

    fn eval_expr_inner(
        &mut self,
        expr: &ExprInstruction,
        out: &mut OutputWriter,
    ) -> ExecutionResult<Number> {
        match expr {
            ExprInstruction::Number(x) => {
                Number::parse(x, self.ibase).ok_or("invalid digit for the current ibase".into())
            }
            ExprInstruction::GetRegister(reg) => match reg {
                Register::Scale => Ok(self.scale.into()),
                Register::IBase => Ok(self.ibase.into()),
                Register::OBase => Ok(self.obase.into()),
            },
            ExprInstruction::Named(named) => self.read_named(named, out),
            ExprInstruction::Builtin { function, arg } => match function {
                BuiltinFunction::Length => Ok(self.eval_expr(arg, out)?.length().into()),
                BuiltinFunction::Sqrt => self
                    .eval_expr(arg, out)?
                    .sqrt(self.scale)
                    .map_err(ExecutionError::from),
                BuiltinFunction::Scale => Ok(self.eval_expr(arg, out)?.scale().into()),
            },
            ExprInstruction::PreIncrement(named) => {
                let value = self.write_named(named, out)?;
                value.inc();
                Ok(value.clone())
            }
            ExprInstruction::PreDecrement(named) => {
                let value = self.write_named(named, out)?;
                value.dec();
                Ok(value.clone())
            }
            ExprInstruction::PostIncrement(named) => {
                let value = self.write_named(named, out)?;
                let result = value.clone();
                value.inc();
                Ok(result)
            }
            ExprInstruction::PostDecrement(named) => {
                let value = self.write_named(named, out)?;
                let result = value.clone();
                value.dec();
                Ok(result)
            }
            ExprInstruction::Call { name, args } => {
                let ic = self.instruction_counter;
                self.instruction_counter = 0;
                let call_result = self.call_function(*name, args, out);
                self.instruction_counter = ic;
                call_result
            }
            ExprInstruction::Assignment { named, value } => {
                let value = self.eval_expr(value, out)?;
                self.write_named(named, out)?.clone_from(&value);
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
                    _ => self.eval_expr(value, out)?,
                };

                self.set_register(*register, &value)?;
                Ok(value)
            }
            ExprInstruction::IncrementRegister { register, prefix } => {
                self.step_register(*register, 1, *prefix)
            }
            ExprInstruction::DecrementRegister { register, prefix } => {
                self.step_register(*register, -1, *prefix)
            }
            ExprInstruction::UnaryMinus(expr) => Ok(self.eval_expr(expr, out)?.negate()),
            ExprInstruction::Add(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)?.add(&self.eval_expr(rhs, out)?))
            }
            ExprInstruction::Sub(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)?.sub(&self.eval_expr(rhs, out)?))
            }
            ExprInstruction::Mul(lhs, rhs) => Ok(self
                .eval_expr(lhs, out)?
                .mul(&self.eval_expr(rhs, out)?, self.scale)),
            ExprInstruction::Div(lhs, rhs) => Ok(self
                .eval_expr(lhs, out)?
                .div(&self.eval_expr(rhs, out)?, self.scale)?),
            ExprInstruction::Mod(lhs, rhs) => self
                .eval_expr(lhs, out)?
                .modulus(&self.eval_expr(rhs, out)?, self.scale)
                .map_err(ExecutionError::from),
            ExprInstruction::Pow(lhs, rhs) => self
                .eval_expr(lhs, out)?
                .pow(&self.eval_expr(rhs, out)?, self.scale)
                .map_err(ExecutionError::from),
        }
    }

    fn eval_condition(
        &mut self,
        condition: &ConditionInstruction,
        out: &mut OutputWriter,
    ) -> ExecutionResult<bool> {
        match condition {
            ConditionInstruction::Expr(expr) => self.eval_expr(expr, out).map(|val| !val.is_zero()),
            ConditionInstruction::Eq(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? == self.eval_expr(rhs, out)?)
            }
            ConditionInstruction::Ne(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? != self.eval_expr(rhs, out)?)
            }
            ConditionInstruction::Lt(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? < self.eval_expr(rhs, out)?)
            }
            ConditionInstruction::Gt(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? > self.eval_expr(rhs, out)?)
            }
            ConditionInstruction::Leq(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? <= self.eval_expr(rhs, out)?)
            }
            ConditionInstruction::Geq(lhs, rhs) => {
                Ok(self.eval_expr(lhs, out)? >= self.eval_expr(rhs, out)?)
            }
        }
    }

    fn eval_stmt(
        &mut self,
        stmt: &StmtInstruction,
        out: &mut OutputWriter,
    ) -> ExecutionResult<ControlFlow> {
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
                let value = self.eval_expr(expr, out)?;
                return Ok(ControlFlow::Return(value));
            }
            StmtInstruction::If {
                condition,
                instruction_count,
                body,
            } => {
                stmt_instruction_count = *instruction_count + 1;
                if self.eval_condition(condition, out)? {
                    // count the condition
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt, out)?;
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
                'while_loop: while self.eval_condition(condition, out)? {
                    // count the condition
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt, out)?;
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
                self.eval_expr(init, out)?;
                'for_loop: while self.eval_condition(condition, out)? {
                    // count init condition and update
                    self.instruction_counter += 1;
                    for stmt in body {
                        let control_flow = self.eval_stmt(stmt, out)?;
                        if control_flow == ControlFlow::Break {
                            break 'for_loop;
                        }
                        if control_flow != ControlFlow::None {
                            return Ok(control_flow);
                        }
                    }
                    // reset the instruction counter to the start of the loop
                    self.instruction_counter = instruction_counter_start;
                    self.eval_expr(update, out)?;
                }
            }
            StmtInstruction::String(s) => out.write_text(s)?,
            StmtInstruction::Expr(expr) => {
                let value = self.eval_expr(expr, out)?;
                if should_print(expr) {
                    out.write_text(&value.to_string(self.obase))?;
                    out.write_text("\n")?;
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

    pub fn exec(&mut self, program: Program, out: &mut OutputWriter) -> ExecutionResult<()> {
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
                    return Ok(());
                }

                self.functions[name_index(name)] = function;
            } else {
                let control_flow = self.eval_stmt(&stmt, out).map_err(|e| {
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
                if contains_quit(&stmt) || self.has_quit {
                    self.has_quit = true;
                    return Ok(());
                }
            }
        }
        Ok(())
    }

    pub fn has_quit(&self) -> bool {
        self.has_quit
    }
}

#[cfg(test)]
impl Interpreter {
    /// Run a program, collecting its output for tests that assert on it.
    fn exec_to_string(&mut self, program: Program) -> ExecutionResult<String> {
        let mut buffer = Vec::new();
        {
            let mut out = OutputWriter::new(&mut buffer);
            self.exec(program, &mut out)?;
        }
        Ok(String::from_utf8(buffer).expect("bc output is UTF-8"))
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(vec![StmtInstruction::Quit].into())
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
            .exec_to_string(
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
        let output = interpreter.exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
            .exec_to_string(Program {
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
