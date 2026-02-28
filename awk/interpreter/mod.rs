//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use array::Array;
use builtins::{builtin_match, builtin_sprintf, call_simple_builtin, print_to_string, sprintf};
use io::{
    EmptyRecordReader, FileStream, ReadFiles, ReadPipes, RecordReader, RecordSeparator,
    StdinRecordReader, WriteFiles, WritePipes,
};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use record::{ere_escape_char, is_valid_record_index, FieldSeparator, FieldsState, Record};
use stack::{
    compare_op, numeric_op, ArrayElementRef, ArrayIterator, ExecutionResult, Stack, StackValue,
};
use string::AwkString;
use value::{AwkRefType, AwkValue, AwkValueRef, AwkValueVariant};

use crate::compiler::{escape_string_contents, is_valid_number};
use crate::program::{
    Action, BuiltinFunction, Constant, Function, OpCode, Pattern, Program, SpecialVar,
};
use crate::regex::Regex;
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::Write;
use std::iter;
use std::rc::Rc;
use std::time::SystemTime;

mod array;
mod builtins;
mod format;
mod io;
mod record;
mod stack;
mod string;
mod value;

#[cfg(test)]
mod tests;

const STACK_SIZE: usize = 2048;

pub(crate) fn bool_to_f64(p: bool) -> f64 {
    if p {
        1.0
    } else {
        0.0
    }
}

pub(crate) fn strtod(s: &str) -> f64 {
    lexical::parse_partial_with_options::<f64, _, { lexical::format::C_STRING }>(
        s,
        &lexical::ParseFloatOptions::default(),
    )
    .map(|(val, _)| val)
    .unwrap_or(0.0)
}

pub(crate) fn is_integer(num: f64) -> bool {
    num.is_finite() && num.fract() == 0.0
}

pub(crate) fn swap_with_default<T: Default>(value: &mut T) -> T {
    let mut result = T::default();
    std::mem::swap(&mut result, value);
    result
}

pub(crate) fn maybe_numeric_string<S: Into<AwkString>>(str: S) -> AwkString {
    let mut str = str.into();
    let numeric_string = is_valid_number(str.as_str().trim().trim_start_matches(['+', '-']));
    str.is_numeric = numeric_string;
    str
}

struct GlobalEnv {
    convfmt: AwkString,
    fs: FieldSeparator,
    ofs: AwkString,
    ors: AwkString,
    ofmt: AwkString,
    rs: RecordSeparator,
    nr: u32,
    fnr: u32,
    nf: usize,
    /// Cached FS combined with newline for paragraph mode (RS="").
    /// Invalidated when FS or RS changes.
    paragraph_fs_cache: Option<FieldSeparator>,
}

impl GlobalEnv {
    fn set(&mut self, var: SpecialVar, value: &mut AwkValue) -> Result<(), String> {
        let as_string = |value: &mut AwkValue| value.clone().scalar_to_string(&self.convfmt);
        match var {
            SpecialVar::Convfmt => self.convfmt = as_string(value)?,
            SpecialVar::Fs => {
                self.fs = as_string(value)?.try_into()?;
                self.paragraph_fs_cache = None;
            }
            SpecialVar::Ofmt => self.ofmt = as_string(value)?,
            SpecialVar::Ofs => self.ofs = as_string(value)?,
            SpecialVar::Ors => self.ors = as_string(value)?,
            SpecialVar::Rs => {
                self.rs = as_string(value)?.try_into()?;
                self.paragraph_fs_cache = None;
            }
            SpecialVar::Nr => self.nr = value.scalar_as_f64() as u32,
            SpecialVar::Fnr => self.fnr = value.scalar_as_f64() as u32,
            SpecialVar::Nf => self.nf = value.scalar_as_f64() as usize,
            _ => {
                // not needed
            }
        }

        Ok(())
    }
}

impl Default for GlobalEnv {
    fn default() -> Self {
        Self {
            convfmt: AwkString::from("%.6g"),
            fs: FieldSeparator::Default,
            ofs: AwkString::from(" "),
            ors: AwkString::from("\n"),
            ofmt: AwkString::from("%.6g"),
            rs: RecordSeparator::Char(b'\n'),
            nr: 1,
            fnr: 1,
            nf: 0,
            paragraph_fs_cache: None,
        }
    }
}

impl GlobalEnv {
    /// Returns the effective FS for the current record. In paragraph mode
    /// (RS=""), newline is always a field separator per POSIX, so the FS is
    /// combined with `\n`. The result is cached and only recomputed when
    /// FS or RS changes.
    fn effective_fs(&mut self) -> Result<&FieldSeparator, String> {
        if !matches!(self.rs, RecordSeparator::Null) {
            return Ok(&self.fs);
        }
        if let Some(ref cached) = self.paragraph_fs_cache {
            return Ok(cached);
        }
        let combined = match &self.fs {
            FieldSeparator::Default | FieldSeparator::Null => None,
            FieldSeparator::Char(c) => {
                let escaped = ere_escape_char(*c as char);
                let pattern = format!("\n|{}", escaped);
                Some(FieldSeparator::Ere(Rc::new(Regex::new(
                    CString::new(pattern).map_err(|e| e.to_string())?,
                )?)))
            }
            FieldSeparator::Ere(re) => {
                let pattern = format!("\n|{}", re.pattern());
                Some(FieldSeparator::Ere(Rc::new(Regex::new(
                    CString::new(pattern).map_err(|e| e.to_string())?,
                )?)))
            }
        };
        match combined {
            Some(fs) => {
                self.paragraph_fs_cache = Some(fs);
                Ok(self.paragraph_fs_cache.as_ref().unwrap())
            }
            None => Ok(&self.fs),
        }
    }
}

struct Interpreter {
    globals: Vec<AwkValueRef>,
    constants: Vec<Constant>,
    write_files: WriteFiles,
    read_files: ReadFiles,
    write_pipes: WritePipes,
    read_pipes: ReadPipes,
    rand_seed: u64,
    rng: SmallRng,
}

fn stack_trace(error: String, stack: Stack) -> String {
    let mut result = format!("runtime error: {}\ncall trace:\n", error);
    let error_location = stack.source_locations[stack.ip as usize];
    writeln!(
        result,
        "=> {} at {}:{}:{}",
        stack.current_function_name,
        stack.current_function_file,
        error_location.line,
        error_location.column
    )
    .expect("error writing to string");
    for frame in stack.call_frames.iter().rev() {
        let source_location = frame.source_locations[frame.ip as usize];
        writeln!(
            result,
            "=> {} at {}:{}:{}",
            frame.function_name, frame.function_file, source_location.line, source_location.column
        )
        .expect("error writing to string");
    }

    result
}

impl Interpreter {
    fn run(
        &mut self,
        action: &Action,
        functions: &[Function],
        record: &mut Record,
        stack: &mut [StackValue],
        global_env: &mut GlobalEnv,
        current_file: &mut dyn RecordReader,
    ) -> Result<ExecutionResult, String> {
        let mut stack = Stack::new(action, stack);
        match self.run_internal(functions, record, &mut stack, global_env, current_file) {
            Err(err) => Err(stack_trace(err, stack)),
            Ok(result) => Ok(result),
        }
    }

    fn run_internal<'a>(
        &mut self,
        functions: &'a [Function],
        record: &Record,
        stack: &mut Stack<'a, 'a>,
        global_env: &mut GlobalEnv,
        current_file: &mut dyn RecordReader,
    ) -> Result<ExecutionResult, String> {
        // # Safety
        // To meat the requirements of stacked borrows (as checked by miri),
        // the `globals` member of `Interpreter` cannot be
        // borrowed mutably, otherwise dereferencing the pointers
        // to global values in the stack would be unsound.
        let mut fields_state = FieldsState::Ok;
        while let Some(instruction) = stack.next_instruction() {
            let mut ip_increment: isize = 1;
            match instruction {
                OpCode::Add => {
                    numeric_op!(stack, +);
                }
                OpCode::Sub => {
                    numeric_op!(stack,  -);
                }
                OpCode::Mul => {
                    numeric_op!(stack,  *);
                }
                OpCode::Div => {
                    numeric_op!(stack,  /);
                }
                OpCode::Mod => {
                    numeric_op!(stack,  %);
                }
                OpCode::Pow => {
                    let rhs = stack.pop_scalar_value()?.scalar_as_f64();
                    let lhs = stack.pop_scalar_value()?.scalar_as_f64();
                    stack.push_value(lhs.powf(rhs))?;
                }
                OpCode::Le => {
                    compare_op!(stack, &global_env.convfmt, <=);
                }
                OpCode::Lt => {
                    compare_op!(stack, &global_env.convfmt, <);
                }
                OpCode::Ge => {
                    compare_op!(stack, &global_env.convfmt, >=);
                }
                OpCode::Gt => {
                    compare_op!(stack, &global_env.convfmt, >);
                }
                OpCode::Eq => {
                    compare_op!(stack, &global_env.convfmt, ==);
                }
                OpCode::Ne => {
                    compare_op!(stack, &global_env.convfmt, !=);
                }
                OpCode::Match => {
                    let ere = stack.pop_value().into_ere()?;
                    let string = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let result = ere.matches(&string.try_into()?);
                    stack.push_value(bool_to_f64(result))?;
                }
                OpCode::Concat => {
                    let rhs = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let mut lhs = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    lhs.concat(&rhs);
                    stack.push_value(lhs)?;
                }
                OpCode::In => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    let result = array.contains(&key);
                    stack.push_value(bool_to_f64(result))?;
                }
                OpCode::Negate => {
                    let value = stack.pop_scalar_value()?.scalar_as_f64();
                    stack.push_value(-value)?;
                }
                OpCode::Not => {
                    let value = stack.pop_scalar_value()?.scalar_as_bool();
                    stack.push_value(bool_to_f64(!value))?;
                }
                OpCode::PostInc => {
                    let lvalue = stack.pop_ref();
                    lvalue.ensure_value_is_scalar()?;
                    let expr_result = lvalue.scalar_as_f64();
                    fields_state = lvalue.assign(expr_result + 1.0, global_env)?;
                    stack.push_value(expr_result)?;
                }
                OpCode::PostDec => {
                    let lvalue = stack.pop_ref();
                    lvalue.ensure_value_is_scalar()?;
                    let expr_result = lvalue.scalar_as_f64();
                    fields_state = lvalue.assign(expr_result - 1.0, global_env)?;
                    stack.push_value(expr_result)?;
                }
                OpCode::PreInc => {
                    let lvalue = stack.pop_ref();
                    lvalue.ensure_value_is_scalar()?;
                    let expr_result = lvalue.scalar_as_f64() + 1.0;
                    fields_state = lvalue.assign(expr_result, global_env)?;
                    stack.push_value(expr_result)?;
                }
                OpCode::PreDec => {
                    let lvalue = stack.pop_ref();
                    lvalue.ensure_value_is_scalar()?;
                    let expr_result = lvalue.scalar_as_f64() - 1.0;
                    fields_state = lvalue.assign(expr_result, global_env)?;
                    stack.push_value(expr_result)?;
                }
                OpCode::CreateGlobalIterator(index) => {
                    let iter_var = stack.pop_ref();
                    iter_var.ensure_value_is_scalar()?;
                    let iter_var = iter_var as *mut AwkValue;
                    let array = self.globals[index as usize].get();
                    let key_iter = unsafe { &mut *array }.as_array()?.key_iter();
                    // both iter_var and array are valid until the stack value is popped.
                    // The first from stack invariance, the second because its a global,
                    // so it will outlive the stack.
                    unsafe {
                        stack.push(StackValue::Iterator(ArrayIterator {
                            iter_var,
                            array,
                            key_iter,
                        }))?
                    };
                }
                OpCode::CreateLocalIterator(index) => {
                    let iter_var = stack.pop_ref();
                    iter_var.ensure_value_is_scalar()?;
                    let iter_var = iter_var as *mut AwkValue;
                    let array = stack
                        .get_mut_value_ptr(index as usize)
                        .expect("invalid local index");
                    // has to be valid, by stack invariance
                    let key_iter = unsafe { &mut *array }.as_array()?.key_iter();
                    // both iter_var and array are valid, at least until this stack value is popped.
                    unsafe {
                        stack.push(StackValue::Iterator(ArrayIterator {
                            iter_var,
                            array,
                            key_iter,
                        }))?
                    };
                }
                OpCode::AdvanceIterOrJump(offset) => {
                    // if the top of the stack is not an iterator
                    // the code is malformed
                    let mut iter = stack.pop().expect("empty stack").unwrap_array_iterator();
                    // The pointer value is valid by stack invariance
                    let array = unsafe { &mut *iter.array }.as_array()?;
                    if let Some(key) = array.key_iter_next(&mut iter.key_iter) {
                        unsafe {
                            // `iter_var` is a valid by stack invariance
                            *iter.iter_var = key.to_string().into();
                            // we only modified the key iterator, so `iter` is still valid
                            stack.push(StackValue::Iterator(iter))?;
                        }
                    } else {
                        ip_increment = offset as isize;
                    }
                }
                OpCode::AsNumber => {
                    let val = stack.pop_scalar_value()?;
                    stack.push_value(val.scalar_as_f64())?;
                }
                OpCode::GetGlobal(index) => unsafe {
                    // globals outlive the stack, so this is safe even if the global is an array
                    stack.push(StackValue::from_var(self.globals[index as usize].get()))?
                },
                OpCode::GetLocal(index) => {
                    let value = stack
                        .get_mut_value_ptr(index as usize)
                        .expect("invalid local index");
                    // this value is valid until the stack value at `index` is popped
                    // so this preserves the stack invariance
                    unsafe { stack.push(StackValue::from_var(value))? };
                }
                OpCode::GetField => {
                    let index = stack.pop_scalar_value()?.scalar_as_f64() as usize;
                    is_valid_record_index(index)?;
                    // fields are never arrays, so this is always safe
                    unsafe { stack.push_value((*record.fields[index].get()).clone())? };
                }
                OpCode::IndexArrayGetValue => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    let element = array.get_value(key.into())?.clone();
                    stack.push_value(element)?
                }
                OpCode::GlobalScalarRef(index) => unsafe {
                    // globals outlive the stack, so this is safe
                    stack.push_ref(self.globals[index as usize].get())?
                },
                OpCode::LocalScalarRef(index) => {
                    let value = stack
                        .get_mut_value_ptr(index as usize)
                        .expect("invalid local index");
                    // this value is valid until the stack value at `index` is popped
                    // so this preserves the stack invariance
                    unsafe { stack.push_ref(value)? };
                }
                OpCode::FieldRef => {
                    let index = stack.pop_scalar_value()?.scalar_as_f64() as usize;
                    is_valid_record_index(index)?;
                    // fields live longer than the stack, so this is safe
                    unsafe { stack.push_ref(record.fields[index].get())? };
                }
                OpCode::IndexArrayGetRef => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = unsafe { stack.pop().expect("empty stack").unwrap_ptr() };
                    // safe by type invariance
                    let value_index = unsafe { &mut *array }
                        .as_array()?
                        .get_value_index(key.into())?;
                    // array is valid at least until this stack value is popped by stack invariance,
                    // so this is safe
                    unsafe {
                        stack.push(StackValue::ArrayElementRef(ArrayElementRef {
                            array,
                            value_index,
                        }))?
                    };
                }
                OpCode::Assign => {
                    let value = stack.pop_scalar_value()?;
                    let lvalue = stack.pop_ref();
                    lvalue.ensure_value_is_scalar()?;
                    fields_state = lvalue.assign(value.clone(), global_env)?;
                    stack.push_value(value)?;
                }
                OpCode::DeleteElement => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    array.delete(&key);
                }
                OpCode::ClearArray => {
                    let array = stack.pop_ref().as_array()?;
                    array.clear();
                }
                OpCode::JumpIfFalse(offset) => {
                    let condition = stack.pop_scalar_value()?.scalar_as_bool();
                    if !condition {
                        ip_increment = offset as isize;
                    }
                }
                OpCode::JumpIfTrue(offset) => {
                    let condition = stack.pop_scalar_value()?.scalar_as_bool();
                    if condition {
                        ip_increment = offset as isize;
                    }
                }
                OpCode::Jump(offset) => {
                    ip_increment = offset as isize;
                }
                OpCode::Call(id) => {
                    stack.call_function(&functions[id as usize]);
                    ip_increment = 0;
                }
                OpCode::CallBuiltin { function, argc } => match function {
                    BuiltinFunction::Match => {
                        let (start, len) = builtin_match(stack, global_env)?;
                        // borrowing `self.globas` mutably here breaks the stacked borrows rules
                        // so we have to use unsafe code to get around that
                        unsafe {
                            *self.globals[SpecialVar::Rstart as usize].get() = start.into();
                            *self.globals[SpecialVar::Rlength as usize].get() = len.into();
                        }
                    }
                    BuiltinFunction::RedirectedPrintfTruncate
                    | BuiltinFunction::RedirectedPrintfAppend
                    | BuiltinFunction::RedirectedPrintTruncate
                    | BuiltinFunction::RedirectedPrintAppend => {
                        let filename = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        let is_printf = matches!(
                            function,
                            BuiltinFunction::RedirectedPrintfTruncate
                                | BuiltinFunction::RedirectedPrintfAppend
                        );
                        let str = if is_printf {
                            builtin_sprintf(stack, argc - 1, global_env)?
                        } else {
                            print_to_string(stack, argc - 1, global_env)?
                        };
                        let is_append = matches!(
                            function,
                            BuiltinFunction::RedirectedPrintfAppend
                                | BuiltinFunction::RedirectedPrintAppend
                        );
                        self.write_files.write(&filename, &str, is_append)?;
                    }
                    BuiltinFunction::RedirectedPrintPipe
                    | BuiltinFunction::RedirectedPrintfPipe => {
                        let command = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        let str = if function == BuiltinFunction::RedirectedPrintPipe {
                            print_to_string(stack, argc - 1, global_env)?
                        } else {
                            builtin_sprintf(stack, argc - 1, global_env)?
                        };
                        self.write_pipes.write(command, str)?;
                    }
                    BuiltinFunction::Close => {
                        let filename = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        self.write_files.close_file(&filename);
                        self.read_files.close_file(&filename);
                        self.write_pipes.close_pipe(&filename);
                        self.read_pipes.close_pipe(&filename);
                    }
                    BuiltinFunction::FFlush => {
                        let expr_str = if argc == 1 {
                            stack
                                .pop_scalar_value()?
                                .scalar_to_string(&global_env.convfmt)?
                        } else {
                            AwkString::default()
                        };
                        let result = if expr_str.is_empty() {
                            let stdout_ok = std::io::Write::flush(&mut std::io::stdout()).is_ok();
                            stdout_ok
                                && self.write_files.flush_all()
                                && self.write_pipes.flush_all()
                        } else {
                            self.write_files.flush_file(&expr_str)
                                && self.write_pipes.flush_file(&expr_str)
                        };
                        stack.push_value(if result { 0.0 } else { -1.0 })?;
                    }
                    BuiltinFunction::GetLine => {
                        let var = stack.pop_ref();
                        if let Some(next_record) = current_file.read_next_record(&global_env.rs)? {
                            fields_state =
                                var.assign(maybe_numeric_string(next_record), global_env)?;
                            // borrowing `self.globas` mutably here breaks the stacked borrows rules
                            // so we have to use unsafe code to get around that
                            let nr = unsafe { &mut *self.globals[SpecialVar::Nr as usize].get() };
                            nr.assign(global_env.nr as f64 + 1.0, global_env)?;
                            let fnr = unsafe { &mut *self.globals[SpecialVar::Fnr as usize].get() };
                            fnr.assign(global_env.fnr as f64 + 1.0, global_env)?;
                            stack.push_value(1.0)?;
                        } else {
                            stack.push_value(0.0)?;
                        }
                    }
                    BuiltinFunction::GetLineFromFile | BuiltinFunction::GetLineFromPipe => {
                        let filename = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        let var = stack.pop_ref();
                        let maybe_next_record = if function == BuiltinFunction::GetLineFromFile {
                            self.read_files.read_next_record(filename, &global_env.rs)?
                        } else {
                            self.read_pipes.read_next_record(filename, &global_env.rs)?
                        };
                        if let Some(next_record) = maybe_next_record {
                            fields_state =
                                var.assign(maybe_numeric_string(next_record), global_env)?;
                            stack.push_value(1.0)?;
                        } else {
                            stack.push_value(0.0)?;
                        }
                    }
                    BuiltinFunction::Rand => {
                        let rand = self.rng.gen_range(0.0..1.0);
                        stack.push_value(rand)?;
                    }
                    BuiltinFunction::Srand => {
                        let seed = if argc == 1 {
                            stack.pop_scalar_value()?.scalar_as_f64() as u64
                        } else {
                            SystemTime::now()
                                .duration_since(SystemTime::UNIX_EPOCH)
                                .expect("time went backwards")
                                .as_secs()
                        };
                        stack.push_value(self.rand_seed as f64)?;
                        self.rand_seed = seed;
                        self.rng = SmallRng::seed_from_u64(self.rand_seed);
                    }
                    other => fields_state = call_simple_builtin(other, argc, stack, global_env)?,
                },
                OpCode::PushConstant(index) => match self.constants[index as usize].clone() {
                    Constant::Number(num) => stack.push_value(num)?,
                    Constant::String(s) => stack.push_value(AwkString::from(s))?,
                    Constant::Regex(ere) => {
                        stack.push_value(AwkValue::from_ere(ere, &record.record.borrow()))?
                    }
                },
                OpCode::PushOne => {
                    stack.push_value(1.0)?;
                }
                OpCode::PushZero => {
                    stack.push_value(0.0)?;
                }
                OpCode::PushUninitialized => {
                    stack.push_value(AwkValue::uninitialized())?;
                }
                OpCode::PushUninitializedScalar => {
                    stack.push_value(AwkValue::uninitialized_scalar())?;
                }
                OpCode::Dup => {
                    // there has to be a value, otherwise the code is malformed
                    let mut val = stack.pop().unwrap();
                    // val is valid at least until the value preceding it is popped
                    // (by stack invariance), so this is safe
                    unsafe {
                        stack
                            .push(val.duplicate())
                            .expect("failed to push a popped value");
                        stack.push(val)?;
                    }
                }
                OpCode::Pop => {
                    stack.pop();
                }
                OpCode::Next => return Ok(ExecutionResult::Next),
                OpCode::NextFile => return Ok(ExecutionResult::NextFile),
                OpCode::Exit => {
                    let exit_code = stack.pop_scalar_value()?.scalar_as_f64();
                    return Ok(ExecutionResult::Exit(exit_code as i32));
                }
                OpCode::Return => {
                    let return_value = stack.pop_scalar_value()?;
                    stack.restore_caller();
                    stack.push_value(return_value)?;
                }
                OpCode::Invalid => panic!("invalid opcode"),
            }
            match fields_state {
                FieldsState::Ok => {
                    // no need to recompute anything
                }
                FieldsState::RecordChanged => {
                    // there are no active field references at this point, so this is safe
                    unsafe { record.recompute_fields(global_env)? };
                    let nf = unsafe { &mut *self.globals[SpecialVar::Nf as usize].get() };
                    nf.assign(record.get_last_field() as f64, global_env)?;
                }
                FieldsState::FieldChanged { changed_field } => {
                    // there are no active field references at this point, so this is safe
                    unsafe { record.recompute_record(global_env, changed_field, false)? };
                    let nf = unsafe { &mut *self.globals[SpecialVar::Nf as usize].get() };
                    nf.assign(record.get_last_field() as f64, global_env)?;
                }
                FieldsState::NfChanged => unsafe {
                    record.recompute_record(global_env, global_env.nf, true)?;
                },
            }
            fields_state = FieldsState::Ok;
            stack.ip += ip_increment;
        }
        Ok(ExecutionResult::Expression(
            stack
                .pop()
                // there are no active references to stack values at this point, so this is safe
                .map(|sv| unsafe { sv.into_owned() })
                .unwrap_or_default(),
        ))
    }

    fn new(args: Array, env: Array, constants: Vec<Constant>, program_globals: usize) -> Self {
        let mut globals = (0..SpecialVar::Count as usize + program_globals)
            .map(|_| AwkValueRef::new(AwkValue::uninitialized()))
            .collect::<Vec<AwkValueRef>>();

        *globals[SpecialVar::Argc as usize].get_mut() = AwkValue::from(args.len() as f64)
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Argc));
        *globals[SpecialVar::Argv as usize].get_mut() =
            AwkValue::from(args).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Argv));
        *globals[SpecialVar::Convfmt as usize].get_mut() = AwkValue::from("%.6g".to_string())
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Convfmt));
        *globals[SpecialVar::Environ as usize].get_mut() =
            AwkValue::from(env).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Environ));
        *globals[SpecialVar::Filename as usize].get_mut() = AwkValue::from("-".to_string())
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Filename));
        *globals[SpecialVar::Fnr as usize].get_mut() =
            AwkValue::from(0.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Fnr));
        *globals[SpecialVar::Fs as usize].get_mut() =
            AwkValue::from(" ").into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Fs));
        *globals[SpecialVar::Nf as usize].get_mut() =
            AwkValue::from(0.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nf));
        *globals[SpecialVar::Nr as usize].get_mut() =
            AwkValue::from(0.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nr));
        *globals[SpecialVar::Ofmt as usize].get_mut() = AwkValue::from("%.6g".to_string())
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ofmt));
        *globals[SpecialVar::Ofs as usize].get_mut() =
            AwkValue::from(" ".to_string()).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ofs));
        *globals[SpecialVar::Ors as usize].get_mut() = AwkValue::from("\n".to_string())
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ors));
        *globals[SpecialVar::Rlength as usize].get_mut() =
            AwkValue::from(0.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rlength));
        *globals[SpecialVar::Rs as usize].get_mut() =
            AwkValue::from("\n".to_string()).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rs));
        *globals[SpecialVar::Rstart as usize].get_mut() =
            AwkValue::from(0.0).into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rstart));
        *globals[SpecialVar::Subsep as usize].get_mut() = AwkValue::from(" ".to_string())
            .into_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Subsep));

        Self {
            globals,
            constants,
            write_files: WriteFiles::default(),
            read_files: ReadFiles::default(),
            write_pipes: WritePipes::default(),
            read_pipes: ReadPipes::default(),
            rand_seed: 0,
            rng: SmallRng::seed_from_u64(0),
        }
    }
}

fn is_valid_variable(s: &str) -> bool {
    s.chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn parse_assignment(s: &str) -> Option<(&str, &str)> {
    let (lhs, rhs) = s.split_once('=')?;
    if is_valid_variable(lhs) {
        Some((lhs, rhs))
    } else {
        None
    }
}

fn set_globals_with_assignment_arguments(
    interpreter: &mut Interpreter,
    globals: &HashMap<String, u32>,
    global_env: &mut GlobalEnv,
    assignments: &[String],
) -> Result<(), String> {
    assignments
        .iter()
        .filter_map(|s| parse_assignment(s))
        .filter_map(|(var, value)| globals.get(var).copied().map(|index| (index, value)))
        .try_for_each(|(global_index, value)| {
            let value = escape_string_contents(value)?;
            interpreter.globals[global_index as usize]
                .get_mut()
                .assign(maybe_numeric_string(value), global_env)
                .expect("failed to assign value");
            Ok(())
        })
}

pub fn interpret(
    program: Program,
    args: &[String],
    assignments: &[String],
    separator: Option<String>,
) -> Result<i32, String> {
    let args = iter::once(("0".to_string(), AwkValue::from("awk")))
        .chain(args.iter().enumerate().map(|(index, s)| {
            (
                (index + 1).to_string(),
                maybe_numeric_string(s.as_str()).into(),
            )
        }))
        .collect();

    let env = std::env::vars()
        .map(|(k, v)| (k, maybe_numeric_string(v)))
        .collect();

    let mut stack = iter::repeat_with(|| StackValue::Invalid)
        .take(STACK_SIZE)
        .collect::<Vec<StackValue>>();
    let mut current_record = Record::default();
    let mut interpreter = Interpreter::new(args, env, program.constants, program.globals_count);
    let mut global_env = GlobalEnv::default();
    let mut range_pattern_started = vec![false; program.rules.len()];
    let mut return_value = 0;

    set_globals_with_assignment_arguments(
        &mut interpreter,
        &program.globals,
        &mut global_env,
        assignments,
    )?;

    if let Some(separator) = separator {
        interpreter.globals[SpecialVar::Fs as usize]
            .get_mut()
            .assign(AwkString::from(separator), &mut global_env)?;
    }

    for action in program.begin_actions {
        let begin_result = interpreter.run(
            &action,
            &program.functions,
            &mut current_record,
            &mut stack,
            &mut global_env,
            &mut EmptyRecordReader::default(),
        )?;
        if let ExecutionResult::Exit(val) = begin_result {
            return_value = val;
            break;
        }
    }

    if program.rules.is_empty() && program.end_actions.is_empty() {
        return Ok(return_value);
    }

    let mut current_arg_index = 1;
    let mut input_read = false;
    'file_loop: loop {
        let argc = interpreter.globals[SpecialVar::Argc as usize]
            .get_mut()
            .scalar_as_f64() as usize;

        let arg = if current_arg_index >= argc {
            if input_read {
                break;
            } else {
                "-".into()
            }
        } else {
            interpreter.globals[SpecialVar::Argv as usize]
                .get_mut()
                .as_array()
                .expect("ARGV is not an array")
                .get_value(current_arg_index.to_string().into())
                // there cannot be active iterators at this point, so this is safe
                .unwrap()
                .clone()
                .scalar_to_string(&global_env.convfmt)?
        };

        if arg.is_empty() {
            current_arg_index += 1;
            continue;
        }

        if let Some((var, value)) = parse_assignment(&arg) {
            if let Some(&global_index) = program.globals.get(var) {
                interpreter.globals[global_index as usize]
                    .get_mut()
                    .assign(
                        maybe_numeric_string(escape_string_contents(value)?),
                        &mut global_env,
                    )?;
            }
            current_arg_index += 1;
            continue;
        }

        interpreter.globals[SpecialVar::Filename as usize]
            .get_mut()
            .value = AwkValueVariant::String(maybe_numeric_string(arg.clone()));

        let reader: &mut dyn RecordReader = if arg.as_str() == "-" {
            &mut StdinRecordReader::default()
        } else {
            &mut FileStream::open(&arg)?
        };

        // at this point we know that some input will be read
        input_read = true;

        global_env.fnr = 1;
        'record_loop: while let Some(record) = reader.read_next_record(&global_env.rs)? {
            let fs = global_env.effective_fs()?;
            current_record.reset(record, fs)?;
            interpreter.globals[SpecialVar::Nf as usize].get_mut().value =
                AwkValue::from(current_record.get_last_field() as f64).value;
            global_env.nf = current_record.get_last_field();

            interpreter.globals[SpecialVar::Fnr as usize]
                .get_mut()
                .value = AwkValue::from(global_env.fnr as f64).value;
            interpreter.globals[SpecialVar::Nr as usize].get_mut().value =
                AwkValue::from(global_env.nr as f64).value;

            for (i, rule) in program.rules.iter().enumerate() {
                let should_execute = match &rule.pattern {
                    Pattern::All => true,
                    Pattern::Expr(expr) => interpreter
                        .run(
                            expr,
                            &program.functions,
                            &mut current_record,
                            &mut stack,
                            &mut global_env,
                            reader,
                        )?
                        .expr_to_bool(),
                    Pattern::Range { start, end } => {
                        if range_pattern_started[i] {
                            let should_end = !interpreter
                                .run(
                                    end,
                                    &program.functions,
                                    &mut current_record,
                                    &mut stack,
                                    &mut global_env,
                                    reader,
                                )?
                                .expr_to_bool();
                            range_pattern_started[i] = should_end;
                            // range is inclusive
                            true
                        } else {
                            let should_start = interpreter
                                .run(
                                    start,
                                    &program.functions,
                                    &mut current_record,
                                    &mut stack,
                                    &mut global_env,
                                    reader,
                                )?
                                .expr_to_bool();
                            range_pattern_started[i] = should_start;
                            should_start
                        }
                    }
                };
                if should_execute {
                    let rule_result = interpreter.run(
                        &rule.action,
                        &program.functions,
                        &mut current_record,
                        &mut stack,
                        &mut global_env,
                        reader,
                    )?;
                    match rule_result {
                        ExecutionResult::Next => break,
                        ExecutionResult::NextFile => {
                            global_env.fnr += 1;
                            global_env.nr += 1;
                            break 'record_loop;
                        }
                        ExecutionResult::Exit(val) => {
                            return_value = val;
                            break 'file_loop;
                        }
                        ExecutionResult::Expression(_) => {}
                    }
                }
            }

            global_env.fnr += 1;
            global_env.nr += 1;
        }

        current_arg_index += 1;
    }

    for action in program.end_actions {
        let end_result = interpreter.run(
            &action,
            &program.functions,
            &mut current_record,
            &mut stack,
            &mut global_env,
            &mut EmptyRecordReader::default(),
        )?;
        if let ExecutionResult::Exit(val) = end_result {
            return_value = val;
            break;
        }
    }

    Ok(return_value)
}
