//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::HashMap;

use crate::program::{BuiltinFunction, Constant, Function, OpCode, Program, SpecialVar};

use clap::arg;
use std::fmt::Write;

fn get_or_insert(array: &mut HashMap<String, ScalarValue>, key: String) -> &mut ScalarValue {
    array.entry(key).or_insert(ScalarValue::Uninitialized)
}

#[derive(Debug, Clone, PartialEq)]
enum ScalarValue {
    Number(f64),
    String(String),
    Uninitialized,
}

impl From<Constant> for ScalarValue {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Number(n) => ScalarValue::Number(n),
            Constant::String(s) => ScalarValue::String(s),
            _ => todo!(),
        }
    }
}

impl ScalarValue {
    fn as_f64_or_err(&self) -> Result<f64, String> {
        match self {
            ScalarValue::Number(n) => Ok(*n),
            ScalarValue::String(s) => s.parse().map_err(|e| todo!()),
            ScalarValue::Uninitialized => Ok(0.0),
        }
    }

    fn as_f64_or_none(&self) -> Option<f64> {
        match self {
            ScalarValue::Number(n) => Some(*n),
            ScalarValue::String(s) => s.parse().ok(),
            ScalarValue::Uninitialized => Some(0.0),
        }
    }

    fn to_string(&self) -> String {
        match self {
            ScalarValue::Number(n) => n.to_string(),
            ScalarValue::String(s) => s.clone(),
            ScalarValue::Uninitialized => String::new(),
        }
    }

    fn is_true(&self) -> bool {
        match self {
            ScalarValue::Number(n) => *n != 0.0,
            ScalarValue::String(s) => !s.is_empty(),
            ScalarValue::Uninitialized => false,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum GlobalValue {
    Scalar(ScalarValue),
    Array(HashMap<String, ScalarValue>),
    Uninitialized,
}

impl GlobalValue {
    fn unwrap_scalar(&self) -> &ScalarValue {
        match self {
            GlobalValue::Scalar(scalar) => scalar,
            _ => unreachable!("expected scalar value"),
        }
    }
}

impl From<ScalarValue> for GlobalValue {
    fn from(value: ScalarValue) -> Self {
        GlobalValue::Scalar(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Reference {
    GlobalVarRef(usize),
    GlobalArrayRef(usize),
    FieldRef(usize),
    LocalVarRef(usize),
    LocalArrayRef(usize),
    TempArray(usize),
}

#[derive(Debug, Clone, PartialEq)]
enum StackValue {
    Scalar(ScalarValue),
    Reference(Reference),
    Uninitialized,
}

impl From<Constant> for StackValue {
    fn from(value: Constant) -> Self {
        ScalarValue::from(value).into()
    }
}

impl From<ScalarValue> for StackValue {
    fn from(value: ScalarValue) -> Self {
        StackValue::Scalar(value)
    }
}

impl From<Reference> for StackValue {
    fn from(value: Reference) -> Self {
        StackValue::Reference(value)
    }
}

struct CallFrame<'i> {
    ip: usize,
    bp: usize,
    last_temp_array: usize,
    instructions: &'i [OpCode],
}

struct Interpreter {
    globals: Vec<GlobalValue>,
    constants: Vec<Constant>,
    stack: Vec<StackValue>,
    fields: Vec<ScalarValue>,
    temp_arrays: Vec<HashMap<String, ScalarValue>>,
    bp: usize,
}

macro_rules! numeric_op {
    ($s:ident, $op:tt) => {
        let rhs = $s.pop_scalar()?.as_f64_or_err()?;
        let lhs = $s.pop_scalar()?.as_f64_or_err()?;
        $s.push(ScalarValue::Number(lhs $op rhs));
    };
}

macro_rules! compare_op {
    ($s:ident, $op:tt) => {
        let rhs = $s.pop_scalar()?;
        let lhs = $s.pop_scalar()?;
        match (lhs, rhs) {
            (ScalarValue::Number(lhs), ScalarValue::Number(rhs)) => {
                $s.push(ScalarValue::Number((lhs $op rhs) as i32 as f64));
            }
            (ScalarValue::String(lhs), ScalarValue::String(rhs)) => {
                $s.push(ScalarValue::Number((lhs $op rhs) as i32 as f64));
            }
            (lhs, rhs) => {
                let lhs_num = lhs.as_f64_or_none();
                let rhs_num = rhs.as_f64_or_none();
                if let (Some(lhs), Some(rhs)) = (lhs_num, rhs_num) {
                    $s.push(ScalarValue::Number((lhs $op rhs) as i32 as f64));
                } else {
                    $s.push(ScalarValue::Number((lhs.to_string() $op rhs.to_string()) as i32 as f64));
                }
            }
        }
    };
}

impl Interpreter {
    fn pop(&mut self) -> StackValue {
        self.stack.pop().expect("stack underflow")
    }

    fn push<V: Into<StackValue>>(&mut self, val: V) {
        self.stack.push(val.into());
    }

    fn get_from_stack_mut(&mut self, index: usize) -> &mut StackValue {
        &mut self.stack[self.bp + index]
    }

    fn get_from_stack(&self, index: usize) -> &StackValue {
        &self.stack[self.bp + index]
    }

    fn get_array_element(&mut self, global_index: usize) -> Result<ScalarValue, String> {
        let key = self.pop_scalar()?.to_string();
        match &mut self.globals[global_index] {
            GlobalValue::Array(map) => Ok(get_or_insert(map, key).clone()),
            global @ GlobalValue::Uninitialized => {
                let new_map = HashMap::from([(key, ScalarValue::Uninitialized)]);
                *global = GlobalValue::Array(new_map);
                Ok(ScalarValue::Uninitialized)
            }
            _ => Err("scalar used in array context".to_string()),
        }
    }

    fn get_array_element_mut(&mut self, global_index: usize) -> Result<&mut ScalarValue, String> {
        let key = self.pop_scalar()?.to_string();
        match &mut self.globals[global_index] {
            GlobalValue::Array(map) => Ok(get_or_insert(map, key)),
            global @ GlobalValue::Uninitialized => {
                *global = GlobalValue::Array(HashMap::new());
                match global {
                    GlobalValue::Array(map) => Ok(get_or_insert(map, key)),
                    _ => unreachable!(),
                }
            }
            _ => Err("scalar used in array context".to_string()),
        }
    }

    fn deref(&mut self, reference: Reference) -> Result<ScalarValue, String> {
        match reference {
            Reference::GlobalVarRef(idx) => match &self.globals[idx] {
                GlobalValue::Scalar(scalar) => Ok(scalar.clone()),
                GlobalValue::Uninitialized => {
                    self.globals[idx] = ScalarValue::Uninitialized.into();
                    Ok(ScalarValue::Uninitialized)
                }
                _ => Err("array used in scalar context".to_string()),
            },
            Reference::GlobalArrayRef(idx) => self.get_array_element(idx),
            Reference::FieldRef(index) => {
                if let Some(value) = self.fields.get(index) {
                    return Ok(value.clone());
                } else {
                    self.fields.resize(index + 1, ScalarValue::Uninitialized);
                    self.globals[SpecialVar::Nf as usize] =
                        ScalarValue::Number(index as f64 + 1.0).into();
                    Ok(ScalarValue::Uninitialized)
                }
            }
            Reference::LocalVarRef(idx) => match self.get_from_stack_mut(idx) {
                StackValue::Scalar(scalar) => Ok(scalar.clone()),
                value @ StackValue::Uninitialized => {
                    *value = ScalarValue::Uninitialized.into();
                    Ok(ScalarValue::Uninitialized)
                }
                _ => Err("array used in scalar context".to_string()),
            },
            Reference::LocalArrayRef(idx) => match &mut self.stack[idx + self.bp] {
                StackValue::Reference(Reference::GlobalArrayRef(global_index)) => {
                    let global_index = *global_index;
                    self.get_array_element(global_index)
                }
                StackValue::Reference(Reference::TempArray(temp_idx)) => {
                    let temp_idx = *temp_idx;
                    let key = self.pop_scalar()?.to_string();
                    Ok(get_or_insert(&mut self.temp_arrays[temp_idx], key).clone())
                }
                value @ StackValue::Uninitialized => {
                    let index = self.temp_arrays.len();
                    *value = Reference::TempArray(index).into();
                    let key = self.pop_scalar()?.to_string();
                    self.temp_arrays
                        .push(HashMap::from([(key, ScalarValue::Uninitialized)]));
                    Ok(ScalarValue::Uninitialized)
                }
                _ => Err("scalar used in array context".to_string()),
            },
            Reference::TempArray(idx) => {
                let key = self.pop_scalar()?.to_string();
                Ok(get_or_insert(&mut self.temp_arrays[idx], key).clone())
            }
        }
    }

    fn stack_value_to_scalar(&mut self, value: StackValue) -> Result<ScalarValue, String> {
        match value {
            StackValue::Scalar(val) => Ok(val),
            StackValue::Reference(reference) => self.deref(reference),
            StackValue::Uninitialized => Ok(ScalarValue::Uninitialized),
        }
    }

    fn pop_scalar(&mut self) -> Result<ScalarValue, String> {
        let value = self.pop();
        self.stack_value_to_scalar(value)
    }

    fn pop_scalar_ref(&mut self) -> Result<&mut ScalarValue, String> {
        match self.pop() {
            StackValue::Reference(reference) => match reference {
                Reference::GlobalVarRef(idx) => match &mut self.globals[idx] {
                    GlobalValue::Scalar(scalar) => Ok(scalar),
                    global @ GlobalValue::Uninitialized => {
                        *global = ScalarValue::Uninitialized.into();
                        match global {
                            GlobalValue::Scalar(scalar) => Ok(scalar),
                            _ => unreachable!(),
                        }
                    }
                    _ => Err("array used in scalar context".to_string()),
                },
                Reference::GlobalArrayRef(idx) => self.get_array_element_mut(idx),
                Reference::LocalVarRef(idx) => match &mut self.stack[idx] {
                    StackValue::Scalar(scalar) => Ok(scalar),
                    _ => Err("array used in scalar context".to_string()),
                },
                Reference::LocalArrayRef(idx) => match self.stack[idx] {
                    StackValue::Reference(Reference::GlobalArrayRef(global_index)) => {
                        self.get_array_element_mut(global_index)
                    }
                    StackValue::Reference(Reference::TempArray(temp_idx)) => {
                        let key = self.pop_scalar()?.to_string();
                        Ok(get_or_insert(&mut self.temp_arrays[temp_idx], key))
                    }
                    _ => Err("scalar used in array context".to_string()),
                },
                Reference::FieldRef(idx) => {
                    if self.fields.len() > idx {
                        Ok(&mut self.fields[idx])
                    } else {
                        self.fields.resize(idx + 1, ScalarValue::Uninitialized);
                        self.globals[SpecialVar::Nf as usize] =
                            ScalarValue::Number(idx as f64 + 1.0).into();
                        Ok(&mut self.fields[idx])
                    }
                }
                Reference::TempArray(_) => {
                    unreachable!("temp arrays should only be accessed through LocalArrayRef")
                }
            },
            _ => panic!("trying to pop a value as reference"),
        }
    }

    fn get_array_ref(
        &mut self,
        value: StackValue,
    ) -> Result<&mut HashMap<String, ScalarValue>, String> {
        let array_ref = if let StackValue::Reference(array_ref) = value {
            array_ref
        } else {
            panic!("expected reference");
        };

        match array_ref {
            Reference::GlobalArrayRef(id) => match &mut self.globals[id] {
                GlobalValue::Array(map) => Ok(map),
                global @ GlobalValue::Uninitialized => {
                    *global = GlobalValue::Array(HashMap::new());
                    match global {
                        GlobalValue::Array(map) => Ok(map),
                        _ => unreachable!(),
                    }
                }
                _ => Err("scalar used in array context".to_string()),
            },
            Reference::LocalArrayRef(id) => match self.stack[self.bp + id] {
                StackValue::Reference(Reference::GlobalArrayRef(global_index)) => {
                    match &mut self.globals[global_index] {
                        GlobalValue::Array(map) => Ok(map),
                        global @ GlobalValue::Uninitialized => {
                            *global = GlobalValue::Array(HashMap::new());
                            match global {
                                GlobalValue::Array(map) => Ok(map),
                                _ => unreachable!(),
                            }
                        }
                        _ => Err("scalar used in array context".to_string()),
                    }
                }
                StackValue::Reference(Reference::TempArray(temp_idx)) => {
                    Ok(&mut self.temp_arrays[temp_idx])
                }
                _ => return Err("scalar used in array context".to_string()),
            },
            Reference::TempArray(_) => {
                unreachable!("temp arrays should only be accessed through LocalArrayRef")
            }
            _ => return Err("scalar used in array context".to_string()),
        }
    }

    fn in_op(&mut self) -> Result<(), String> {
        let array_ref = self.pop();
        let key = self.pop_scalar()?.to_string();
        let array = self.get_array_ref(array_ref)?;
        let contained = array.contains_key(&key) as i32 as f64;
        self.push(ScalarValue::Number(contained as i32 as f64));
        Ok(())
    }

    fn delete_op(&mut self) -> Result<(), String> {
        let array_ref = self.pop();
        let key = self.pop_scalar()?.to_string();
        let array = self.get_array_ref(array_ref)?;
        array.remove(&key);
        Ok(())
    }

    fn sprintf(&mut self, argc: u16) -> Result<String, String> {
        let arg_start = self.stack.len() - argc as usize;
        self.stack[arg_start..].reverse();
        let format = self.pop_scalar()?.to_string();
        let mut result = String::with_capacity(format.len());
        let mut remaining_args = argc - 1;
        let mut iter = format.chars();

        let iter_next =
            |iter: &mut std::str::Chars| iter.next().ok_or("invalid format string".to_string());

        let parse_number =
            |next: &mut char, iter: &mut std::str::Chars| -> Result<Option<usize>, String> {
                let mut number = 0;
                loop {
                    match *next {
                        c if c.is_digit(10) => {
                            number = number * 10 + c.to_digit(10).unwrap() as usize;
                        }
                        _ => break,
                    }
                    *next = iter_next(iter)?;
                }
                if number == 0 {
                    Ok(None)
                } else {
                    Ok(Some(number))
                }
            };

        while let Some(c) = iter.next() {
            if c == '%' {
                let mut next = iter_next(&mut iter)?;
                let mut left_justified = false;
                let mut signed = false;
                let mut prefix_space = false;
                let mut alternative_form = false;
                let mut zero_padded = false;
                loop {
                    match next {
                        '-' => left_justified = true,
                        '+' => signed = true,
                        ' ' => prefix_space = true,
                        '#' => alternative_form = true,
                        '0' => zero_padded = true,
                        _ => break,
                    }
                    next = iter_next(&mut iter)?;
                }

                let field_width = parse_number(&mut next, &mut iter)?.unwrap_or(0);

                let precision = if next == '.' {
                    next = iter_next(&mut iter)?;
                    parse_number(&mut next, &mut iter)?.unwrap_or(0)
                } else {
                    usize::MAX
                };

                if next == '%' {
                    result.push('%');
                    continue;
                }
                if remaining_args == 0 {
                    return Err("not enough arguments for format string".to_string());
                }
                remaining_args -= 1;
                match next {
                    c if "aA".contains(c) => {
                        todo!()
                    }
                    c if "diouxX".contains(c) => {
                        todo!()
                    }
                    c if "fF".contains(c) => {
                        todo!()
                    }
                    c if "eE".contains(c) => {
                        todo!()
                    }
                    c if "gG".contains(c) => {
                        todo!()
                    }
                    'c' => {
                        todo!()
                    }
                    's' => {
                        let value = self.pop_scalar()?.to_string();
                        if left_justified {
                            write!(result, "{:<w$.p$}", value, w = field_width, p = precision)
                        } else {
                            write!(result, "{:>w$.p$}", value, w = field_width, p = precision)
                        }
                        .expect("error formatting string");
                    }
                    _ => return Err("invalid conversion specifier".to_string()),
                }
            } else {
                result.push(c);
            }
        }
        Ok(result)
    }

    fn call_builtin(&mut self, function: BuiltinFunction, argc: u16) -> Result<(), String> {
        match function {
            BuiltinFunction::Atan2 => {
                let y = self.pop_scalar()?.as_f64_or_err()?;
                let x = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(y.atan2(x)));
            }
            BuiltinFunction::Cos => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.cos()));
            }
            BuiltinFunction::Sin => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.sin()));
            }
            BuiltinFunction::Exp => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.exp()));
            }
            BuiltinFunction::Log => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.ln()));
            }
            BuiltinFunction::Sqrt => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.sqrt()));
            }
            BuiltinFunction::Int => {
                let value = self.pop_scalar()?.as_f64_or_err()?;
                self.push(ScalarValue::Number(value.trunc()));
            }
            BuiltinFunction::Rand => {
                todo!()
            }
            BuiltinFunction::Srand => {
                todo!()
            }
            BuiltinFunction::Gsub => {
                todo!()
            }
            BuiltinFunction::Index => {
                let t = self.pop_scalar()?.to_string();
                let s = self.pop_scalar()?.to_string();
                let index = s.find(&t).map(|i| i as f64 + 1.0).unwrap_or(0.0);
                self.push(ScalarValue::Number(index));
            }
            BuiltinFunction::Length => {
                if argc == 0 {
                    let value = self.fields[0].to_string();
                    self.push(ScalarValue::Number(value.len() as f64));
                } else {
                    let value = self.pop_scalar()?.to_string();
                    self.push(ScalarValue::Number(value.len() as f64));
                }
            }
            BuiltinFunction::Match => {
                todo!()
            }
            BuiltinFunction::Split => {
                let s = self.pop_scalar()?.to_string();
                let array_ref = self.pop();
                let separator = if argc == 2 {
                    self.globals[SpecialVar::Fs as usize]
                        .unwrap_scalar()
                        .to_string()
                } else {
                    assert_eq!(argc, 3);
                    todo!()
                };
                let array = self.get_array_ref(array_ref)?;
                array.clear();
                for (i, part) in s.split(&separator).enumerate() {
                    array.insert(i.to_string(), ScalarValue::String(part.to_string()));
                }
                let n = array.len();
                self.push(ScalarValue::Number(n as f64));
            }
            BuiltinFunction::Sprintf => {
                let result = self.sprintf(argc)?;
                self.push(ScalarValue::String(result));
            }
            BuiltinFunction::Sub => {
                todo!()
            }
            BuiltinFunction::Substr => {
                let n = if argc == 2 {
                    usize::MAX
                } else {
                    self.pop_scalar()?.as_f64_or_err()? as usize
                };
                let m = self.pop_scalar()?.as_f64_or_err()? as usize;
                let s = self.pop_scalar()?.to_string();
                let substr = s.chars().skip(m).take(n).collect::<String>();
                self.push(ScalarValue::String(substr));
            }
            BuiltinFunction::ToLower => {
                let value = self.pop_scalar()?.to_string();
                self.push(ScalarValue::String(value.to_lowercase()));
            }
            BuiltinFunction::ToUpper => {
                let value = self.pop_scalar()?.to_string();
                self.push(ScalarValue::String(value.to_uppercase()));
            }
            BuiltinFunction::Close => {
                todo!()
            }
            BuiltinFunction::GetLine => {
                todo!()
            }
            BuiltinFunction::System => {
                todo!()
            }
            BuiltinFunction::Print => {
                let field_separator = self.globals[SpecialVar::Ofs as usize]
                    .unwrap_scalar()
                    .to_string();
                let record_separator = self.globals[SpecialVar::Ors as usize]
                    .unwrap_scalar()
                    .to_string();
                let mut output = String::new();
                for i in 0..argc {
                    let value = self.pop_scalar()?.to_string();
                    output.push_str(&value);
                    if i < argc - 1 {
                        output.push_str(&field_separator);
                    }
                }
                print!("{}{}", output, record_separator);
            }
            BuiltinFunction::Printf => {
                todo!()
            }
            BuiltinFunction::Count => unreachable!("invalid builtin function"),
        }
        Ok(())
    }

    fn run(
        &mut self,
        main: &[OpCode],
        functions: &[Function],
        record: &[String],
    ) -> Result<(), String> {
        self.globals[SpecialVar::Nf as usize] = ScalarValue::Number(record.len() as f64).into();
        self.fields.resize(record.len(), ScalarValue::Uninitialized);
        for (i, field) in record.iter().enumerate() {
            self.fields[i] = ScalarValue::String(field.clone());
        }

        let mut ip = 0i64;
        let mut instructions = main;
        let mut call_frames = vec![];
        while (ip as usize) < instructions.len() {
            let mut ip_increment = 1i64;
            match instructions[ip as usize] {
                OpCode::Add => {
                    numeric_op!(self, +);
                }
                OpCode::Sub => {
                    numeric_op!(self, -);
                }
                OpCode::Mul => {
                    numeric_op!(self, *);
                }
                OpCode::Div => {
                    numeric_op!(self, /);
                }
                OpCode::Mod => {
                    numeric_op!(self, %);
                }
                OpCode::Pow => {
                    let rhs = self.pop_scalar()?.as_f64_or_err()?;
                    let lhs = self.pop_scalar()?.as_f64_or_err()?;
                    self.push(ScalarValue::Number(lhs.powf(rhs)));
                }
                OpCode::Le => {
                    compare_op!(self, <=);
                }
                OpCode::Lt => {
                    compare_op!(self, <);
                }
                OpCode::Ge => {
                    compare_op!(self, >=);
                }
                OpCode::Gt => {
                    compare_op!(self, >);
                }
                OpCode::Eq => {
                    compare_op!(self, ==);
                }
                OpCode::Ne => {
                    compare_op!(self, !=);
                }
                OpCode::Match => todo!(),
                OpCode::NotMatch => todo!(),
                OpCode::Concat => {
                    let rhs = self.pop_scalar()?.to_string();
                    let lhs = self.pop_scalar()?.to_string();
                    self.push(ScalarValue::String(lhs + &rhs));
                }
                OpCode::In => self.in_op()?,
                OpCode::Negate => {
                    let value = self.pop_scalar()?.as_f64_or_err()?;
                    self.push(ScalarValue::Number(-value));
                }
                OpCode::Not => {
                    let value = !self.pop_scalar()?.is_true();
                    self.push(ScalarValue::Number(value as i32 as f64));
                }
                OpCode::PostInc => {
                    let reference = self.pop_scalar_ref()?;
                    let num = reference.as_f64_or_err()?;
                    *reference = ScalarValue::Number(num + 1.0);
                    self.push(ScalarValue::Number(num));
                }
                OpCode::PostDec => {
                    let reference = self.pop_scalar_ref()?;
                    let num = reference.as_f64_or_err()?;
                    *reference = ScalarValue::Number(num - 1.0);
                    self.push(ScalarValue::Number(num));
                }
                OpCode::PreInc => {
                    let reference = self.pop_scalar_ref()?;
                    let num = reference.as_f64_or_err()? + 1.0;
                    *reference = ScalarValue::Number(num);
                    self.push(ScalarValue::Number(num));
                }
                OpCode::PreDec => {
                    let reference = self.pop_scalar_ref()?;
                    let num = reference.as_f64_or_err()? - 1.0;
                    *reference = ScalarValue::Number(num);
                    self.push(ScalarValue::Number(num));
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::ArrayRef(id) => {
                    self.push(StackValue::Reference(Reference::GlobalArrayRef(
                        id as usize,
                    )));
                }
                OpCode::VarRef(idx) => {
                    self.push(StackValue::Reference(Reference::GlobalVarRef(idx as usize)));
                }
                OpCode::FieldRef => {
                    let index = self.pop_scalar()?.as_f64_or_err()?;
                    if index.is_sign_negative() {
                        return Err("negative field index".to_string());
                    }
                    self.push(StackValue::Reference(Reference::FieldRef(index as usize)));
                }
                OpCode::Assign => {
                    let value = self.pop_scalar()?;
                    let reference = self.pop_scalar_ref()?;
                    *reference = value.clone();
                    self.push(value);
                }
                OpCode::LocalVarRef(idx) => {
                    self.push(StackValue::Reference(Reference::LocalVarRef(idx as usize)));
                }
                OpCode::LocalArrayRef(idx) => {
                    self.push(Reference::LocalArrayRef(idx as usize));
                }
                OpCode::Delete => self.delete_op()?,
                OpCode::JumpIfFalse(offset) => {
                    if !self.pop_scalar()?.is_true() {
                        ip_increment = offset as i64;
                    }
                }
                OpCode::JumpIfTrue(offset) => {
                    if self.pop_scalar()?.is_true() {
                        ip_increment = offset as i64;
                    }
                }
                OpCode::Jump(offset) => {
                    ip_increment = offset as i64;
                }
                OpCode::Call { id, argc } => {
                    let function = &functions[id as usize];
                    self.bp = self.stack.len() - argc as usize;
                    call_frames.push(CallFrame {
                        ip: ip as usize,
                        bp: self.bp,
                        last_temp_array: self.temp_arrays.len(),
                        instructions,
                    });
                    instructions = &function.instructions;
                    ip = 0;
                    ip_increment = 0;
                }
                OpCode::CallBuiltin { function, argc } => {
                    self.call_builtin(function, argc)?;
                }
                OpCode::PushConstant(idx) => {
                    self.push(self.constants[idx as usize].clone());
                }
                OpCode::PushOne => {
                    self.push(ScalarValue::Number(1.0));
                }
                OpCode::PushUninitialized => {
                    self.push(StackValue::Uninitialized);
                }
                OpCode::Return => {
                    let return_value = self.pop_scalar()?;
                    let frame = call_frames.pop().expect("return outside of function");
                    self.bp = frame.bp;
                    self.stack.truncate(self.bp);
                    self.temp_arrays.truncate(frame.last_temp_array);
                    self.push(return_value);
                    instructions = frame.instructions;
                    ip = frame.ip as i64;
                }
                OpCode::Invalid => panic!("invalid opcode"),
                other => todo!("{:?}", other),
            }
            ip += ip_increment;
        }
        Ok(())
    }

    fn new(
        args: HashMap<String, String>,
        env: HashMap<String, String>,
        constants: Vec<Constant>,
        program_globals: usize,
    ) -> Self {
        let mut globals =
            vec![GlobalValue::Uninitialized; SpecialVar::Count as usize + program_globals];

        globals[SpecialVar::Argc as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Argv as usize] = GlobalValue::Array(HashMap::new());
        globals[SpecialVar::Convfmt as usize] =
            GlobalValue::Scalar(ScalarValue::String("%.6g".to_string()));
        globals[SpecialVar::Environ as usize] = GlobalValue::Array(HashMap::new());
        globals[SpecialVar::Filename as usize] =
            GlobalValue::Scalar(ScalarValue::String("-".to_string()));
        globals[SpecialVar::Fnr as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Fs as usize] =
            GlobalValue::Scalar(ScalarValue::String(" ".to_string()));
        globals[SpecialVar::Nf as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Nr as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Ofmt as usize] =
            GlobalValue::Scalar(ScalarValue::String("%.6g".to_string()));
        globals[SpecialVar::Ofs as usize] =
            GlobalValue::Scalar(ScalarValue::String(" ".to_string()));
        globals[SpecialVar::Ors as usize] =
            GlobalValue::Scalar(ScalarValue::String("\n".to_string()));
        globals[SpecialVar::Rlength as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Rs as usize] =
            GlobalValue::Scalar(ScalarValue::String("\n".to_string()));
        globals[SpecialVar::Rstart as usize] = GlobalValue::Scalar(ScalarValue::Number(0.0));
        globals[SpecialVar::Subsep as usize] =
            GlobalValue::Scalar(ScalarValue::String("\034".to_string()));

        Self {
            globals,
            constants,
            bp: 0,
            stack: vec![],
            fields: vec![],
            temp_arrays: vec![],
        }
    }
}

pub fn interpret(program: Program, files: Vec<String>) -> Result<(), String> {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

    fn interpret_expr(
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
    ) -> ScalarValue {
        let mut interpreter =
            Interpreter::new(HashMap::new(), HashMap::new(), constants, global_count);
        interpreter
            .run(&instructions, &[], &[])
            .expect("error running test");
        interpreter.pop_scalar().unwrap()
    }

    fn interpret_expr_with_record(
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
        record: Vec<String>,
    ) -> ScalarValue {
        let mut interpreter =
            Interpreter::new(HashMap::new(), HashMap::new(), constants, global_count);
        interpreter
            .run(&instructions, &[], &record)
            .expect("error running test");
        interpreter.pop_scalar().unwrap()
    }

    fn test_global(instructions: Vec<OpCode>, constants: Vec<Constant>) -> GlobalValue {
        let mut interpreter = Interpreter::new(HashMap::new(), HashMap::new(), constants, 1);
        interpreter
            .run(&instructions, &[], &[])
            .expect("error running test");
        interpreter.globals[FIRST_GLOBAL_VAR as usize].clone()
    }

    fn interpret_with_functions(
        main: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
        functions: Vec<Function>,
    ) -> ScalarValue {
        let mut interpreter =
            Interpreter::new(HashMap::new(), HashMap::new(), constants, global_count);
        interpreter
            .run(&main, &functions, &[])
            .expect("error running test");
        interpreter.pop_scalar().unwrap()
    }

    fn test_sprintf(format: &str, args: Vec<Constant>) -> String {
        let mut instructions = vec![OpCode::PushConstant(0)];
        let mut constants = vec![Constant::String(format.to_string())];
        let argc = args.len() + 1;
        for (i, c) in args.into_iter().enumerate() {
            instructions.push(OpCode::PushConstant(i as u32 + 1));
            constants.push(c);
        }
        instructions.push(OpCode::CallBuiltin {
            function: BuiltinFunction::Sprintf,
            argc: argc as u16,
        });
        let result = interpret_expr(instructions, constants, 0);
        if let ScalarValue::String(s) = result {
            s
        } else {
            panic!("expected string, got {:?}", result);
        }
    }

    #[test]
    fn test_push_constant() {
        let instructions = vec![OpCode::PushConstant(0)];
        let constant = vec![Constant::Number(1.0)];
        assert_eq!(
            interpret_expr(instructions.clone(), constant, 0),
            ScalarValue::Number(1.0)
        );

        let constant = vec![Constant::String("hello".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::String("hello".to_string())
        );
    }

    #[test]
    fn test_add() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Add,
        ];
        let constant = vec![Constant::Number(1.0), Constant::Number(1.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(2.0)
        );
    }

    #[test]
    fn test_sub() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Sub,
        ];
        let constant = vec![Constant::Number(145.0), Constant::Number(123.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(22.0)
        );
    }

    #[test]
    fn test_mul() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Mul,
        ];
        let constant = vec![Constant::Number(12.0), Constant::Number(12.0)];

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(144.0)
        );
    }

    #[test]
    fn test_div() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Div,
        ];
        let constant = vec![Constant::Number(144.0), Constant::Number(12.0)];

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(12.0)
        );
    }

    #[test]
    fn test_mod() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Mod,
        ];
        let constant = vec![Constant::Number(144.0), Constant::Number(12.0)];

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(0.0)
        );
    }

    #[test]
    fn test_pow() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Pow,
        ];
        let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(8.0)
        );
    }

    #[test]
    fn test_string_concat() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Concat,
        ];
        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::String("world".to_string()),
        ];

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::String("helloworld".to_string())
        );
    }

    #[test]
    fn test_numeric_op_with_numeric_string_args() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Add,
        ];
        let constant = vec![
            Constant::String("1.22".to_string()),
            Constant::String("3.44".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(4.66)
        );
    }

    #[test]
    fn test_compare_same_operand_type() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(1.0)
        );

        let constant = vec![
            Constant::String("abcd".to_string()),
            Constant::String("efgh".to_string()),
        ];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(0.0)
        );
    }

    #[test]
    fn test_compare_number_string() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::String("3.0".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(1.0)
        );

        let constant = vec![Constant::String("abcd".to_string()), Constant::Number(3.0)];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_compare_number_uninitialized() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::VarRef(FIRST_GLOBAL_VAR),
            OpCode::Ge,
        ];
        let constant = vec![Constant::Number(2.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_interpret_in_for_global_array() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            ScalarValue::Number(0.0)
        );

        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_interpret_in_for_local_array_ref() {
        let instructions = vec![
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::LocalArrayRef(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            ScalarValue::Number(0.0)
        );

        let instructions = vec![
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::Pop,
            OpCode::PushConstant(0),
            OpCode::LocalArrayRef(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_negate() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Negate];
        let constant = vec![Constant::Number(456.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(-456.0)
        );
    }

    #[test]
    fn test_not() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Not];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_postinc() {
        let instructions = vec![OpCode::VarRef(FIRST_GLOBAL_VAR), OpCode::PostInc];
        assert_eq!(
            test_global(instructions, vec![]),
            ScalarValue::Number(1.0).into()
        );
    }

    #[test]
    fn test_postdec() {
        let instructions = vec![OpCode::VarRef(FIRST_GLOBAL_VAR), OpCode::PostDec];
        assert_eq!(
            test_global(instructions, vec![]),
            ScalarValue::Number(-1.0).into()
        );
    }

    #[test]
    fn test_preinc() {
        let instructions = vec![OpCode::VarRef(FIRST_GLOBAL_VAR), OpCode::PreInc];
        assert_eq!(
            test_global(instructions, vec![]),
            ScalarValue::Number(1.0).into()
        );
    }

    #[test]
    fn test_predec() {
        let instructions = vec![OpCode::VarRef(FIRST_GLOBAL_VAR), OpCode::PreDec];
        assert_eq!(
            test_global(instructions, vec![]),
            ScalarValue::Number(-1.0).into()
        );
    }

    #[test]
    fn test_assign_to_global_var() {
        let instructions = vec![
            OpCode::VarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constant = vec![Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            ScalarValue::Number(123.0).into()
        );
    }

    #[test]
    fn test_assign_to_array_element() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            GlobalValue::Array(HashMap::from([(
                "key".to_string(),
                ScalarValue::Number(123.0)
            )]))
        );
    }

    #[test]
    fn test_delete_global_array_element_after_insertion() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            GlobalValue::Array(HashMap::new())
        );
    }

    #[test]
    fn test_delete_global_array_element_after_insertion_through_local_ref() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::LocalArrayRef(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            GlobalValue::Array(HashMap::new())
        );
    }

    #[test]
    fn test_delete_from_empty_global_array() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            test_global(instructions, constant),
            GlobalValue::Array(HashMap::new())
        );
    }

    #[test]
    fn test_assign_to_local_var() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::LocalVarRef(0),
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![
            Constant::Number(0.0),
            Constant::String("test string".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::String("test string".to_string())
        );
    }

    #[test]
    fn test_assign_to_array_through_local_ref() {
        let instructions = vec![
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::LocalArrayRef(0),
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            GlobalValue::Array(HashMap::from([(
                "key".to_string(),
                ScalarValue::Number(123.0)
            )]))
        );
    }

    #[test]
    fn test_jump() {
        let instructions = vec![
            OpCode::Jump(2),
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
        ];
        let constant = vec![Constant::Number(1.0), Constant::Number(2.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(2.0)
        );
    }

    #[test]
    fn test_jump_if_false() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::JumpIfFalse(2),
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
        ];
        let constant = vec![
            Constant::String("".to_string()),
            Constant::Number(1.0),
            Constant::Number(2.0),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(2.0)
        );
    }

    #[test]
    fn test_jump_if_true() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::JumpIfTrue(2),
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
        ];
        let constant = vec![
            Constant::Number(1.0),
            Constant::Number(2.0),
            Constant::Number(3.0),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(3.0)
        );
    }

    #[test]
    fn test_call_function_without_args() {
        let main = vec![OpCode::Call { id: 0, argc: 0 }];
        let functions = vec![Function {
            parameters_count: 0,
            instructions: vec![OpCode::PushConstant(0), OpCode::Return],
        }];
        let constant = vec![Constant::String("test".to_string())];
        assert_eq!(
            interpret_with_functions(main, constant, 0, functions),
            ScalarValue::String("test".to_string())
        );
    }

    #[test]
    fn test_call_with_uninitialized_scalar_argument() {
        let main = vec![OpCode::PushUninitialized, OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![OpCode::LocalVarRef(0), OpCode::Return],
        }];
        assert_eq!(
            interpret_with_functions(main, vec![], 1, functions),
            ScalarValue::Uninitialized
        );
    }

    #[test]
    fn test_call_with_uninitialized_array_argument() {
        let main = vec![OpCode::PushUninitialized, OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::PushConstant(0),
                OpCode::LocalArrayRef(0),
                OpCode::Return,
            ],
        }];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_with_functions(main, constant, 1, functions),
            ScalarValue::Uninitialized
        );
    }

    #[test]
    fn test_call_function_with_scalar_argument() {
        let main = vec![OpCode::PushConstant(0), OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![OpCode::LocalVarRef(0), OpCode::PushOne, OpCode::Add],
        }];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(
            interpret_with_functions(main, constant, 0, functions),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_call_function_with_array_argument() {
        let main = vec![
            OpCode::ArrayRef(FIRST_GLOBAL_VAR),
            OpCode::Call { id: 0, argc: 1 },
        ];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::PushConstant(0),
                OpCode::LocalArrayRef(0),
                OpCode::PushOne,
                OpCode::Assign,
            ],
        }];
        let constants = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_with_functions(main, constants, 1, functions),
            ScalarValue::Number(1.0)
        );
    }

    #[test]
    fn test_call_function_with_multiple_scalar_arguments() {
        let main = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(0),
            OpCode::PushConstant(0),
            OpCode::PushConstant(0),
            OpCode::PushConstant(0),
            OpCode::Call { id: 0, argc: 5 },
        ];
        let functions = vec![Function {
            parameters_count: 5,
            instructions: vec![
                OpCode::LocalVarRef(0),
                OpCode::LocalVarRef(1),
                OpCode::LocalVarRef(2),
                OpCode::LocalVarRef(3),
                OpCode::LocalVarRef(4),
                OpCode::Add,
                OpCode::Add,
                OpCode::Add,
                OpCode::Add,
                OpCode::Return,
            ],
        }];
        let constants = vec![Constant::Number(1.0)];
        assert_eq!(
            interpret_with_functions(main, constants, 0, functions),
            ScalarValue::Number(5.0)
        );
    }

    #[test]
    fn test_access_whole_record_field() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::FieldRef];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(
            interpret_expr_with_record(
                instructions,
                constant,
                0,
                vec!["hello".to_string(), "hello".to_string()]
            ),
            ScalarValue::String("hello".to_string())
        );
    }

    #[test]
    fn test_assign_to_out_of_bounds_field() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::FieldRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constants = vec![Constant::Number(9.0)];

        let mut interpreter = Interpreter::new(HashMap::new(), HashMap::new(), constants, 0);
        interpreter
            .run(
                &instructions,
                &[],
                &["test".to_string(), "test".to_string()],
            )
            .unwrap();
        assert_eq!(interpreter.fields.len(), 10);
        assert_eq!(
            interpreter.globals[SpecialVar::Nf as usize],
            ScalarValue::Number(10.0).into()
        );
    }

    #[test]
    fn test_builtin_index() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Index,
                argc: 2,
            },
        ];
        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::String("l".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions.clone(), constant, 0),
            ScalarValue::Number(3.0)
        );

        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::String("z".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(0.0)
        );
    }

    #[test]
    fn test_builtin_length() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Length,
                argc: 1,
            },
        ];
        let constant = vec![Constant::String("hello".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::Number(5.0)
        );

        let instructions = vec![OpCode::CallBuiltin {
            function: BuiltinFunction::Length,
            argc: 0,
        }];
        assert_eq!(
            interpret_expr_with_record(instructions, vec![], 0, vec!["test record".to_string()]),
            ScalarValue::Number(11.0)
        );
    }

    #[test]
    fn test_builtin_substr() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Substr,
                argc: 3,
            },
        ];
        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::Number(1.0),
            Constant::Number(2.0),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::String("el".to_string())
        );

        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Substr,
                argc: 2,
            },
        ];
        let constant = vec![Constant::String("hello".to_string()), Constant::Number(3.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            ScalarValue::String("lo".to_string())
        );
    }

    #[test]
    fn test_builtin_sprintf_with_string_args() {
        assert_eq!(test_sprintf("hello", vec![]), "hello");
        assert_eq!(
            test_sprintf("hello %s", vec![Constant::String("world".to_string())]),
            "hello world"
        );
        assert_eq!(
            test_sprintf(
                "%s:%s:%s",
                vec![
                    Constant::String("a".to_string()),
                    Constant::String("b".to_string()),
                    Constant::String("c".to_string())
                ]
            ),
            "a:b:c"
        );
        assert_eq!(
            test_sprintf("%10s", vec![Constant::String("test".to_string())]),
            "      test"
        );
        assert_eq!(
            test_sprintf("%-10s", vec![Constant::String("test".to_string())]),
            "test      "
        );
        assert_eq!(
            test_sprintf("%.2s", vec![Constant::String("test".to_string())]),
            "te"
        );
        assert_eq!(
            test_sprintf("%.20s", vec![Constant::String("test".to_string())]),
            "test"
        );
        assert_eq!(
            test_sprintf("%10.2s", vec![Constant::String("test".to_string())]),
            "        te"
        );
        assert_eq!(
            test_sprintf("%-10.2s", vec![Constant::String("test".to_string())]),
            "te        "
        );
        assert_eq!(
            test_sprintf("%10.20s", vec![Constant::String("test".to_string())]),
            "      test"
        );
    }
}
