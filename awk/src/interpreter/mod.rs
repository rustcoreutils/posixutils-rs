//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use array::{Array, KeyIterator};
use io::{
    EmptyRecordReader, FileStream, ReadFiles, ReadPipes, RecordReader, RecordSeparator,
    StdinRecordReader, WriteFiles, WritePipes,
};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use string::AwkString;

use crate::compiler::{escape_string_contents, is_valid_number};
use crate::program::{BuiltinFunction, Constant, Function, OpCode, Pattern, Program, SpecialVar};
use crate::regex::Regex;
use format::{
    fmt_write_decimal_float, fmt_write_float_general, fmt_write_hex_float,
    fmt_write_scientific_float, fmt_write_signed, fmt_write_string, fmt_write_unsigned,
    parse_conversion_specifier_args, IntegerFormat,
};
use std::cell::{RefCell, UnsafeCell};
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::Write;
use std::iter;
use std::marker::PhantomData;
use std::rc::Rc;
use std::time::SystemTime;

mod array;
mod format;
mod io;
mod string;

const STACK_SIZE: usize = 2048;

fn bool_to_f64(p: bool) -> f64 {
    if p {
        1.0
    } else {
        0.0
    }
}

fn strtod(s: &str) -> f64 {
    lexical::parse_partial_with_options::<f64, _, { lexical::format::C_STRING }>(
        s,
        &lexical::ParseFloatOptions::default(),
    )
    .map(|(val, _)| val)
    .unwrap_or(0.0)
}

fn gather_values(stack: &mut Stack, count: u16) -> Result<Vec<AwkValue>, String> {
    let mut values = Vec::new();
    for _ in 0..count {
        values.push(stack.pop_scalar_value()?);
    }
    Ok(values)
}

fn print_to_string(
    stack: &mut Stack,
    argc: u16,
    global_env: &GlobalEnv,
) -> Result<AwkString, String> {
    let mut values = Vec::new();
    for _ in 0..argc {
        values.push(
            stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.ofmt)?,
        );
    }
    let mut output = String::new();
    values.iter().skip(1).rev().fold(&mut output, |acc, elem| {
        write!(acc, "{}{}", elem, &global_env.ofs).expect("error writing to string");
        acc
    });
    // there has to be at least an element
    output.push_str(values.first().expect("called print without arguments"));
    output.push_str(&global_env.ors);
    Ok(output.into())
}

fn is_integer(num: f64) -> bool {
    num.is_finite() && num.fract() == 0.0
}

fn swap_with_default<T: Default>(value: &mut T) -> T {
    let mut result = T::default();
    std::mem::swap(&mut result, value);
    result
}

fn maybe_numeric_string<S: Into<AwkString>>(str: S) -> AwkString {
    let mut str = str.into();
    let numeric_string = is_valid_number(
        str.as_str()
            .trim()
            .trim_start_matches(|c| c == '+' || c == '-'),
    );
    str.is_numeric = numeric_string;
    str
}

fn sprintf(
    format_string: &str,
    values: &mut [AwkValue],
    float_format: &str,
) -> Result<AwkString, String> {
    let mut result = String::with_capacity(format_string.len());
    let mut iter = format_string.chars();
    let mut next = iter.next();
    let mut current_arg = values.len();
    while let Some(c) = next {
        match c {
            '%' => {
                let (specifier, args) = parse_conversion_specifier_args(&mut iter)?;
                if specifier == '%' {
                    result.push('%');
                    next = iter.next();
                    continue;
                }

                if current_arg == 0 {
                    return Err("not enough arguments for format string".to_string());
                }
                current_arg -= 1;
                let value = swap_with_default(&mut values[current_arg]);
                match specifier {
                    'd' | 'i' => {
                        let value = value.scalar_as_f64() as i64;
                        fmt_write_signed(&mut result, value, &args);
                    }
                    'u' | 'o' | 'x' | 'X' => {
                        let value = value.scalar_as_f64() as i64;
                        if value.is_negative() {
                            return Err("negative value for unsigned format specifier".to_string());
                        }
                        let format = match specifier {
                            'u' => IntegerFormat::Decimal,
                            'o' => IntegerFormat::Octal,
                            'x' => IntegerFormat::HexLower,
                            'X' => IntegerFormat::HexUpper,
                            _ => unreachable!(),
                        };
                        fmt_write_unsigned(&mut result, value as u64, format, &args);
                    }
                    'a' | 'A' => {
                        let value = value.scalar_as_f64();
                        fmt_write_hex_float(&mut result, value, specifier == 'a', &args);
                    }
                    'f' | 'F' => {
                        let value = value.scalar_as_f64();
                        fmt_write_decimal_float(&mut result, value, specifier == 'f', &args);
                    }
                    'e' | 'E' => {
                        let value = value.scalar_as_f64();
                        fmt_write_scientific_float(&mut result, value, specifier == 'e', &args);
                    }
                    'g' | 'G' => {
                        let value = value.scalar_as_f64();
                        fmt_write_float_general(&mut result, value, specifier == 'g', &args);
                    }
                    'c' => {
                        let value = value.scalar_as_f64() as i64 as u8;
                        result.push(value as char);
                    }
                    's' => {
                        let value = value.scalar_to_string(&float_format)?;
                        fmt_write_string(&mut result, &value, &args);
                    }
                    _ => return Err(format!("unsupported format specifier '{}'", specifier)),
                }
                next = iter.next();
            }
            other => {
                result.push(other);
                next = iter.next();
            }
        }
    }
    Ok(result.into())
}

fn builtin_sprintf(
    stack: &mut Stack,
    argc: u16,
    global_env: &mut GlobalEnv,
) -> Result<AwkString, String> {
    let mut values = gather_values(stack, argc - 1)?;
    let format_string = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    sprintf(&format_string, &mut values, &global_env.convfmt)
}

fn builtin_match(stack: &mut Stack, global_env: &mut GlobalEnv) -> Result<(f64, f64), String> {
    let ere = stack.pop_value().to_ere()?;
    let string = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    // TODO: should look into this unwrap
    let mut locations = ere.match_locations(string.try_into()?);
    let start;
    let len;
    if let Some(first_match) = locations.next() {
        start = first_match.start as i64 + 1;
        len = first_match.end as i64 - start + 1;
    } else {
        start = 0;
        len = -1;
    }
    stack.push_value(start as f64)?;
    Ok((start as f64, len as f64))
}

fn gsub(ere: &Regex, repl: &str, in_str: &str, only_replace_first: bool) -> (AwkString, usize) {
    let mut result = String::with_capacity(in_str.len());
    let mut last_match_end = 0;

    let mut repl_parts = Vec::new();
    let mut current_repl_part = String::new();
    let mut repl_iter = repl.chars();
    while let Some(c) = repl_iter.next() {
        if c == '\\' {
            match repl_iter.next() {
                Some('\\') => current_repl_part.push('\\'),
                Some('&') => current_repl_part.push_str(&in_str[last_match_end..]),
                Some(c) => {
                    current_repl_part.push('\\');
                    current_repl_part.push(c);
                }
                None => {
                    current_repl_part.push('\\');
                    break;
                }
            }
        } else if c == '&' {
            repl_parts.push(current_repl_part);
            current_repl_part = String::new();
        } else {
            current_repl_part.push(c);
        }
    }
    repl_parts.push(current_repl_part);

    let mut num_replacements = 0;
    for m in ere.match_locations(CString::new(in_str).unwrap()) {
        result.push_str(&in_str[last_match_end..m.start]);
        let replaced_string = &in_str[m.start..m.end];
        result.push_str(&repl_parts[0]);
        for part in repl_parts.iter().skip(1) {
            result.push_str(part);
            result.push_str(replaced_string);
        }
        last_match_end = m.end;
        num_replacements += 1;
        if only_replace_first {
            break;
        }
    }
    result.push_str(&in_str[last_match_end..]);
    (result.into(), num_replacements)
}

fn builtin_gsub(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
    is_sub: bool,
) -> Result<FieldsState, String> {
    let repl = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    let ere = stack.pop_value().to_ere()?;
    let in_str = stack.pop_ref();
    in_str.ensure_value_is_scalar()?;
    let (result, count) = gsub(
        &ere,
        &repl,
        &in_str.share().scalar_to_string(&global_env.convfmt)?,
        is_sub,
    );
    let result = in_str.assign(result, global_env);
    stack.push_value(count as f64)?;
    result
}

fn call_simple_builtin(
    function: BuiltinFunction,
    argc: u16,
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
) -> Result<FieldsState, String> {
    match function {
        BuiltinFunction::Atan2 => {
            let y = stack.pop_scalar_value()?.scalar_as_f64();
            let x = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(y.atan2(x))?;
        }
        BuiltinFunction::Cos => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.cos())?;
        }
        BuiltinFunction::Sin => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.sin())?;
        }
        BuiltinFunction::Exp => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.exp())?;
        }
        BuiltinFunction::Log => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.ln())?;
        }
        BuiltinFunction::Sqrt => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.sqrt())?;
        }
        BuiltinFunction::Int => {
            let value = stack.pop_scalar_value()?.scalar_as_f64();
            stack.push_value(value.trunc())?;
        }
        BuiltinFunction::Index => {
            let t = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let index = s
                .as_str()
                .find(t.as_str())
                .map(|i| i as f64 + 1.0)
                .unwrap_or(0.0);
            stack.push_value(index)?;
        }
        BuiltinFunction::Length => {
            let value = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            stack.push_value(value.len() as f64)?;
        }
        BuiltinFunction::Split => {
            let separator = if argc == 2 {
                None
            } else {
                Some(FieldSeparator::Ere(stack.pop_value().to_ere()?))
            };
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let array = stack.pop_ref().as_array()?;
            array.clear();

            split_record(
                s,
                &separator.iter().next().unwrap_or(&global_env.fs),
                |i, s| array.set((i + 1).to_string(), s).map(|_| ()),
            )?;
            let n = array.len();
            stack.push_value(n as f64)?;
        }
        BuiltinFunction::Sprintf => {
            let str = builtin_sprintf(stack, argc, global_env)?;
            stack.push_value(str)?;
        }
        BuiltinFunction::Substr => {
            let n = if argc == 2 {
                usize::MAX
            } else {
                stack.pop_scalar_value()?.scalar_as_f64() as usize
            };
            // the behaviour for values < 1 is not specified. Here we follow what other
            // implementations do
            let m = stack.pop_scalar_value()?.scalar_as_f64().max(1.0) as usize;
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let substr = s.chars().skip(m - 1).take(n).collect::<String>();
            stack.push_value(substr)?;
        }
        BuiltinFunction::ToLower => {
            let value = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            stack.push_value(value.to_lowercase())?;
        }
        BuiltinFunction::ToUpper => {
            let value = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            stack.push_value(value.to_uppercase())?;
        }
        BuiltinFunction::Gsub | BuiltinFunction::Sub => {
            return builtin_gsub(stack, global_env, function == BuiltinFunction::Sub)
        }
        BuiltinFunction::System => {
            let command: CString = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?
                .try_into()?;
            let status = unsafe { libc::system(command.as_ptr()) };
            if status == -1 {
                return Err("system call failed".to_string());
            }
        }
        BuiltinFunction::Print => {
            print!("{}", print_to_string(stack, argc, global_env)?);
        }
        BuiltinFunction::Printf => {
            print!("{}", builtin_sprintf(stack, argc, global_env)?);
        }
        _ => unreachable!("call_simple_builtin was passed an invalid builtin function kind"),
    }
    Ok(FieldsState::Ok)
}

enum FieldSeparator {
    Default,
    Char(u8),
    Ere(Rc<Regex>),
}

/// Splits a record into fields and calls the provided closure for each field.
/// If the record is a numeric string, fields will be numeric strings if appropriate.
fn split_record<S: FnMut(usize, AwkString) -> Result<(), String>>(
    mut record: AwkString,
    field_separator: &FieldSeparator,
    mut store_result: S,
) -> Result<(), String> {
    let is_record_numeric = record.is_numeric;
    let string = |s: &str| -> AwkString {
        if is_record_numeric {
            AwkString::numeric_string(s)
        } else {
            s.into()
        }
    };
    match field_separator {
        FieldSeparator::Default => record
            .trim_start()
            .split_ascii_whitespace()
            .enumerate()
            .try_for_each(|(i, s)| store_result(i, string(s))),
        FieldSeparator::Char(c) => record
            .split(*c as char)
            .enumerate()
            .try_for_each(|(i, s)| store_result(i, string(s))),
        FieldSeparator::Ere(re) => {
            let mut split_start = 0;
            let mut index = 0;
            for separator_range in re.match_locations(record.share().try_into()?) {
                store_result(index, string(&record[split_start..separator_range.start]))?;
                split_start = separator_range.end;
                index += 1;
            }
            store_result(index, string(&record[split_start..]))
        }
    }
}

impl TryFrom<AwkString> for FieldSeparator {
    type Error = String;

    fn try_from(value: AwkString) -> Result<Self, Self::Error> {
        if value.as_str() == " " {
            Ok(FieldSeparator::Default)
        } else if value.len() == 1 {
            Ok(FieldSeparator::Char(*value.as_bytes().first().unwrap()))
        } else {
            let ere = Regex::new(value.try_into()?)?;
            Ok(FieldSeparator::Ere(Rc::from(ere)))
        }
    }
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
}

impl GlobalEnv {
    fn set(&mut self, var: SpecialVar, value: &mut AwkValue) -> Result<(), String> {
        let as_string = |value: &mut AwkValue| value.share().scalar_to_string(&self.convfmt);
        match var {
            SpecialVar::Convfmt => self.convfmt = as_string(value)?,
            SpecialVar::Fs => self.fs = as_string(value)?.try_into()?,
            SpecialVar::Ofmt => self.ofmt = as_string(value)?,
            SpecialVar::Ofs => self.ofs = as_string(value)?,
            SpecialVar::Ors => self.ors = as_string(value)?,
            SpecialVar::Rs => self.rs = as_string(value)?.try_into()?,
            SpecialVar::Nr => self.nr = value.scalar_as_f64() as u32,
            SpecialVar::Fnr => self.fnr = value.scalar_as_f64() as u32,
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
        }
    }
}

enum FieldsState {
    NeedToRecomputeFields,
    NeedToRecomputeRecord { changed_field: usize },
    Ok,
}

struct Record {
    record: RefCell<CString>,
    fields: Vec<AwkValueRef>,
    last_field: RefCell<usize>,
}

impl Record {
    fn reset(&mut self, record: String, field_separator: &FieldSeparator) -> Result<(), String> {
        let previous_last_field = *self.last_field.get_mut();
        let mut last_field = 0;
        let mut record = maybe_numeric_string(record);
        split_record(record.share(), field_separator, |i, s| {
            let field_index = i + 1;
            last_field += 1;
            *self.fields[field_index].get_mut() = AwkValue::field_ref(s, field_index as u16);
            Ok(())
        })
        .expect("error splitting record");
        if last_field < previous_last_field {
            for field in &mut self.fields[last_field..=previous_last_field] {
                field.get_mut().value = AwkValueVariant::UninitializedScalar;
            }
        }
        *self.fields[0].get_mut() = AwkValue::field_ref(record.share(), 0);
        *self.record.get_mut() = record.try_into()?;
        *self.last_field.get_mut() = last_field;
        Ok(())
    }

    /// # Safety
    /// The caller has to ensure that there are no active references to the fields
    unsafe fn recompute_record(
        &self,
        global_env: &GlobalEnv,
        changed_field: usize,
    ) -> Result<(), String> {
        let last_field = self.last_field.borrow().max(changed_field);
        let mut new_record = String::new();
        for field in self.fields.iter().skip(1).take(last_field - 1) {
            let field_str = (*field.get())
                .clone()
                .scalar_to_string(&global_env.convfmt)?;
            write!(new_record, "{}{}", field_str, &global_env.ofs)
                .expect("error writing to string");
        }
        let last_field_str = (*self.fields[last_field].get())
            .clone()
            .scalar_to_string(&global_env.convfmt)?;
        write!(new_record, "{}", last_field_str).expect("error writing to string");
        // TODO: don't know if this is correct. Should the recomputed record be
        // numeric?
        let mut record_str = maybe_numeric_string(new_record);
        *self.fields[0].get() = AwkValue::field_ref(record_str.share(), 0);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    /// # Safety
    /// The caller has to ensure that there are no active references to the fields
    unsafe fn recompute_fields(&self, global_env: &GlobalEnv) -> Result<(), String> {
        let mut last_field = 0;
        let mut record_str = (*self.fields[0].get())
            .to_owned()
            .scalar_to_string(&global_env.convfmt)?;
        split_record(record_str.share(), &global_env.fs, |i, s| {
            let field_index = i + 1;
            last_field += 1;
            *self.fields[field_index].get() = AwkValue::field_ref(s, field_index as u16);
            Ok(())
        })
        .expect("error splitting record");
        *self.fields[0].get() = AwkValue::field_ref(record_str.share(), 0);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    fn get_last_field(&self) -> usize {
        *self.last_field.borrow()
    }
}

impl Default for Record {
    fn default() -> Self {
        let fields = (0..1024)
            .map(|i| {
                AwkValueRef::new(AwkValue::uninitialized_scalar().to_ref(AwkRefType::Field(i)))
            })
            .collect();
        Self {
            record: CString::default().into(),
            // TODO: fix magic number
            fields,
            last_field: 0.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum AwkValueVariant {
    Number(f64),
    String(AwkString),
    Array(Array),
    Regex {
        ere: Rc<Regex>,
        matches_record: bool,
    },
    Uninitialized,
    UninitializedScalar,
}

impl AwkValueVariant {
    fn share(&mut self) -> Self {
        match self {
            AwkValueVariant::String(value) => AwkValueVariant::String(value.share()),
            other => other.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum AwkRefType {
    None,
    Field(u16),
    SpecialGlobalVar(SpecialVar),
}

#[derive(Debug, Clone, PartialEq)]
struct AwkValue {
    value: AwkValueVariant,
    ref_type: AwkRefType,
}

type AwkValueRef = UnsafeCell<AwkValue>;

impl AwkValue {
    fn scalar_as_f64(&self) -> f64 {
        match &self.value {
            AwkValueVariant::Number(x) => *x,
            AwkValueVariant::String(s) => strtod(s.as_str()),
            AwkValueVariant::UninitializedScalar => 0.0,
            AwkValueVariant::Regex { matches_record, .. } => bool_to_f64(*matches_record),
            AwkValueVariant::Array(_) | AwkValueVariant::Uninitialized => {
                panic!("not a scalar")
            }
        }
    }

    fn scalar_as_bool(&self) -> bool {
        match &self.value {
            AwkValueVariant::Number(x) => *x != 0.0,
            AwkValueVariant::String(s) => !s.is_empty(),
            AwkValueVariant::Regex { matches_record, .. } => *matches_record,
            AwkValueVariant::UninitializedScalar => false,
            AwkValueVariant::Array(_) | AwkValueVariant::Uninitialized => {
                panic!("not a scalar")
            }
        }
    }

    fn scalar_to_string(self, num_fmt: &str) -> Result<AwkString, String> {
        match self.value {
            AwkValueVariant::Number(num) => {
                if is_integer(num) {
                    Ok((num as i64).to_string().into())
                } else {
                    sprintf(num_fmt, &mut [num.into()], num_fmt).map(|s| s.into())
                }
            }
            AwkValueVariant::String(s) => Ok(s),
            AwkValueVariant::Regex { matches_record, .. } => {
                Ok(if matches_record { "1" } else { "0" }.into())
            }
            AwkValueVariant::UninitializedScalar => Ok(AwkString::default()),
            AwkValueVariant::Array(_) | AwkValueVariant::Uninitialized => {
                panic!("not a scalar")
            }
        }
    }

    fn ensure_value_is_scalar(&mut self) -> Result<(), String> {
        match &self.value {
            AwkValueVariant::Uninitialized => self.value = AwkValueVariant::UninitializedScalar,
            AwkValueVariant::Array(_) => return Err("array used in scalar context".into()),
            _ => {
                //already a scalar
            }
        }
        Ok(())
    }

    fn as_array(&mut self) -> Result<&mut Array, String> {
        match &mut self.value {
            AwkValueVariant::Array(array) => Ok(array),
            value @ AwkValueVariant::Uninitialized => {
                *value = AwkValueVariant::Array(Array::default());
                if let AwkValueVariant::Array(array) = value {
                    Ok(array)
                } else {
                    unreachable!()
                }
            }
            _ => Err("scalar used in array context".to_string()),
        }
    }

    fn assign<V: Into<Self>>(
        &mut self,
        rhs: V,
        global_env: &mut GlobalEnv,
    ) -> Result<FieldsState, String> {
        let rhs = rhs.into();
        self.value = rhs.value;
        match self.ref_type {
            AwkRefType::SpecialGlobalVar(special_var) => global_env.set(special_var, self)?,
            AwkRefType::Field(index) => {
                return if index == 0 {
                    Ok(FieldsState::NeedToRecomputeFields)
                } else {
                    Ok(FieldsState::NeedToRecomputeRecord {
                        changed_field: index as usize,
                    })
                };
            }
            AwkRefType::None => {}
        }
        Ok(FieldsState::Ok)
    }

    fn to_ref(self, ref_type: AwkRefType) -> Self {
        Self { ref_type, ..self }
    }

    fn to_ere(self) -> Result<Rc<Regex>, String> {
        match self.value {
            AwkValueVariant::Regex { ere, .. } => Ok(ere),
            _ => Err("expected extended regular expression".to_string()),
        }
    }

    fn share(&mut self) -> Self {
        Self {
            value: self.value.share(),
            ref_type: AwkRefType::None,
        }
    }

    fn uninitialized() -> Self {
        Self {
            value: AwkValueVariant::Uninitialized,
            ref_type: AwkRefType::None,
        }
    }

    fn uninitialized_scalar() -> Self {
        Self {
            value: AwkValueVariant::UninitializedScalar,
            ref_type: AwkRefType::None,
        }
    }

    fn from_ere(ere: Rc<Regex>, record: &CString) -> Self {
        let matches_record = ere.matches(record);
        Self {
            value: AwkValueVariant::Regex {
                ere,
                matches_record,
            },
            ref_type: AwkRefType::None,
        }
    }

    fn field_ref<V: Into<AwkValue>>(value: V, field_index: u16) -> Self {
        let mut value = value.into();
        value.to_ref(AwkRefType::Field(field_index))
    }
}

impl Default for AwkValue {
    fn default() -> Self {
        Self::uninitialized()
    }
}

impl From<f64> for AwkValue {
    fn from(value: f64) -> Self {
        Self {
            value: AwkValueVariant::Number(value),
            ref_type: AwkRefType::None,
        }
    }
}

impl From<AwkString> for AwkValue {
    fn from(value: AwkString) -> Self {
        let value = value.into();
        Self {
            value: AwkValueVariant::String(value),
            ref_type: AwkRefType::None,
        }
    }
}

impl From<String> for AwkValue {
    fn from(value: String) -> Self {
        AwkString::from(value).into()
    }
}

impl From<&str> for AwkValue {
    fn from(value: &str) -> Self {
        AwkString::from(value).into()
    }
}

impl From<Array> for AwkValue {
    fn from(value: Array) -> Self {
        Self {
            value: AwkValueVariant::Array(value),
            ref_type: AwkRefType::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ArrayIterator {
    array: *mut AwkValue,
    iter_var: *mut AwkValue,
    key_iter: KeyIterator,
}

#[derive(Debug, Clone, PartialEq)]
enum StackValue {
    Value(AwkValue),
    ValueRef(*mut AwkValue),
    UninitializedRef(*mut AwkValue),
    Iterator(ArrayIterator),
}

impl StackValue {
    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn value_ref(&mut self) -> &mut AwkValue {
        match self {
            StackValue::Value(val) => val,
            StackValue::ValueRef(val_ref) => &mut **val_ref,
            StackValue::UninitializedRef(val_ref) => &mut **val_ref,
            _ => unreachable!("invalid stack value"),
        }
    }

    fn unwrap_ptr(self) -> *mut AwkValue {
        match self {
            StackValue::ValueRef(ptr) => ptr,
            StackValue::UninitializedRef(ptr) => ptr,
            _ => unreachable!("expected lvalue"),
        }
    }

    fn unwrap_array_iterator(self) -> ArrayIterator {
        match self {
            StackValue::Iterator(array_iterator) => array_iterator,
            _ => unreachable!("expected iterator"),
        }
    }

    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn into_owned(self) -> AwkValue {
        match self {
            StackValue::Value(val) => val,
            StackValue::ValueRef(ref_val) => (*ref_val).share(),
            StackValue::UninitializedRef(_) => AwkValue::uninitialized_scalar(),
            _ => unreachable!("invalid stack value"),
        }
    }

    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn ensure_value_is_scalar(&mut self) -> Result<(), String> {
        self.value_ref().ensure_value_is_scalar()
    }

    unsafe fn from_var(value: *mut AwkValue) -> Self {
        let value_ref = &mut *value;
        match value_ref.value {
            AwkValueVariant::Array(_) => StackValue::ValueRef(value),
            AwkValueVariant::Uninitialized => StackValue::UninitializedRef(value),
            _ => StackValue::Value(value_ref.share()),
        }
    }
}

impl From<AwkValue> for StackValue {
    fn from(value: AwkValue) -> Self {
        StackValue::Value(value)
    }
}

struct CallFrame<'i> {
    bp: *mut StackValue,
    sp: *mut StackValue,
    ip: isize,
    instructions: &'i [OpCode],
}

/// # Invariants
/// - `sp` and `bp` are pointers into the same
/// contiguously allocated chunk of memory
/// - `stack_end` is one past the last valid pointer
/// of the allocated memory starting at `bp`
/// - values in the range [`bp`, `sp`) can be accessed safely
struct Stack<'i, 's> {
    ip: isize,
    instructions: &'i [OpCode],
    sp: *mut StackValue,
    bp: *mut StackValue,
    stack_end: *mut StackValue,
    call_frames: Vec<CallFrame<'i>>,
    _stack_lifetime: PhantomData<&'s ()>,
}

/// Safe interface to work with the program stack.
impl<'i, 's> Stack<'i, 's> {
    /// pops the `StackValue` on top of the stack.
    /// # Returns
    /// The top stack value if there is one. `None` otherwise
    fn pop(&mut self) -> Option<StackValue> {
        if self.sp != self.bp {
            let mut value = StackValue::Value(AwkValue::uninitialized());
            self.sp = unsafe { self.sp.sub(1) };
            unsafe { std::mem::swap(&mut value, &mut *self.sp) };
            Some(value)
        } else {
            None
        }
    }

    /// pushes a StackValue on top of the stack
    /// # Errors
    /// returns an error in case of stack overflow
    /// # Safety
    /// `value` has to be valid at least until the value preceding it is popped
    unsafe fn push(&mut self, value: StackValue) -> Result<(), String> {
        if self.sp == self.stack_end {
            Err("stack overflow".to_string())
        } else {
            *self.sp = value.into();
            self.sp = self.sp.add(1);
            Ok(())
        }
    }

    fn pop_scalar_value(&mut self) -> Result<AwkValue, String> {
        let mut value = self.pop().unwrap();
        // safe by type invariance
        unsafe {
            value.ensure_value_is_scalar()?;
            Ok(value.into_owned())
        }
    }

    fn get_mut_value_ptr(&mut self, index: usize) -> Option<*mut AwkValue> {
        if unsafe { self.sp.offset_from(self.bp) } >= index as isize {
            let value = unsafe { &mut *self.bp.add(index) };
            match value {
                StackValue::Value(val) => Some(val),
                StackValue::ValueRef(val_ref) => Some(*val_ref),
                StackValue::UninitializedRef(val_ref) => Some(*val_ref),
                _ => unreachable!("invalid stack value"),
            }
        } else {
            None
        }
    }

    fn pop_value(&mut self) -> AwkValue {
        // safe by type invariance
        unsafe {
            let value = self.pop().unwrap();
            value.into_owned()
        }
    }

    fn pop_ref(&mut self) -> &mut AwkValue {
        // safe by type invariance
        unsafe { &mut *self.pop().unwrap().unwrap_ptr() }
    }

    fn push_value<V: Into<AwkValue>>(&mut self, value: V) -> Result<(), String> {
        // a `StackValue::Value` is always valid, so this is safe
        unsafe { self.push(StackValue::Value(value.into())) }
    }

    /// pushes a reference on the stack
    /// # Safety
    /// `value_ptr` has to be safe to access at least until the value preceding it
    /// on the stack is popped.
    unsafe fn push_ref(&mut self, value_ptr: *mut AwkValue) -> Result<(), String> {
        self.push(StackValue::ValueRef(value_ptr))
    }

    fn next_instruction(&mut self) -> Option<OpCode> {
        self.instructions.get(self.ip as usize).copied()
    }

    fn call_function(&mut self, instructions: &'i [OpCode], argc: usize) {
        unsafe { assert!(self.sp.offset_from(self.bp) >= argc as isize) };
        let new_bp = unsafe { self.sp.sub(argc) };
        let caller_frame = CallFrame {
            bp: self.bp,
            sp: new_bp,
            ip: self.ip,
            instructions: self.instructions,
        };
        self.call_frames.push(caller_frame);
        self.bp = new_bp;
        self.ip = 0;
        self.instructions = instructions;
    }

    fn restore_caller(&mut self) {
        let caller_frame = self
            .call_frames
            .pop()
            .expect("tried to restore caller when there is none");
        self.bp = caller_frame.bp;
        self.sp = caller_frame.sp;
        self.instructions = caller_frame.instructions;
        self.ip = caller_frame.ip;
    }

    fn new(main: &'i [OpCode], stack: &'s mut [StackValue]) -> Self {
        let stack_len = stack.len();
        let bp = stack.as_mut_ptr();
        let stack_end = unsafe { bp.add(stack_len) };
        Self {
            instructions: main,
            ip: 0,
            bp,
            sp: bp,
            stack_end,
            call_frames: Vec::new(),
            _stack_lifetime: PhantomData::default(),
        }
    }
}

enum ExecutionResult {
    Expression(AwkValue),
    Next,
    Exit(i32),
}

impl ExecutionResult {
    fn expr_to_bool(self) -> bool {
        self.unwrap_expr().scalar_as_bool()
    }

    fn unwrap_expr(self) -> AwkValue {
        match self {
            ExecutionResult::Expression(value) => value,
            _ => panic!("execution result is not an expression"),
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

macro_rules! numeric_op {
    ($stack:expr, $op:tt) => {
        let rhs = $stack.pop_scalar_value()?.scalar_as_f64();
        let lhs = $stack.pop_scalar_value()?.scalar_as_f64();
        $stack.push_value(lhs $op rhs)?;
    };
}

macro_rules! compare_op {
    ($stack:expr, $convfmt:expr, $op:tt) => {
        let rhs = $stack.pop_scalar_value()?;
        let lhs = $stack.pop_scalar_value()?;
        match (&lhs.value, &rhs.value) {
            (AwkValueVariant::Number(lhs), AwkValueVariant::Number(rhs)) => {
                $stack.push_value(bool_to_f64(lhs $op rhs))?;
            }
            (AwkValueVariant::String(lhs), AwkValueVariant::String(rhs)) => {
                $stack.push_value(bool_to_f64(lhs.as_str() $op rhs.as_str()))?;
            }
            (AwkValueVariant::Number(lhs), AwkValueVariant::UninitializedScalar) => {
                $stack.push_value(bool_to_f64(*lhs $op 0.0))?;
            }
            (AwkValueVariant::UninitializedScalar, AwkValueVariant::Number(rhs)) => {
                $stack.push_value(bool_to_f64(0.0 $op *rhs))?;
            }
            (AwkValueVariant::String(s), AwkValueVariant::Number(x)) if s.is_numeric => {
                $stack.push_value(bool_to_f64(lhs.scalar_as_f64() $op *x))?;
            }
            (AwkValueVariant::Number(x), AwkValueVariant::String(s)) if s.is_numeric => {
                $stack.push_value(bool_to_f64(*x $op rhs.scalar_as_f64()))?;
            }
            (_, _) => {
                $stack.push_value(bool_to_f64(lhs.scalar_to_string($convfmt)?.as_str() $op rhs.scalar_to_string($convfmt)?.as_str()))?;
            }
        }
    };
}

impl Interpreter {
    fn run(
        &mut self,
        main: &[OpCode],
        functions: &[Function],
        record: &mut Record,
        stack: &mut [StackValue],
        global_env: &mut GlobalEnv,
        current_file: &mut dyn RecordReader,
    ) -> Result<ExecutionResult, String> {
        let mut stack = Stack::new(main, stack);
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
                    let ere = stack.pop_value().to_ere()?;
                    let string = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    // FIXME: remove unwrap
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
                    let key_iter = unsafe { &mut *array }.as_array()?.key_iter();
                    unsafe {
                        stack.push(StackValue::Iterator(ArrayIterator {
                            iter_var,
                            array,
                            key_iter,
                        }))?
                    };
                }
                OpCode::AdvanceIterOrJump(offset) => {
                    let mut iter = stack.pop().unwrap().unwrap_array_iterator();
                    let array = unsafe { &mut *iter.array }.as_array()?;
                    if let Some(key) = array.key_iter_next(&mut iter.key_iter) {
                        unsafe {
                            *iter.iter_var = key.to_string().into();
                        }
                        unsafe {
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
                    stack.push(StackValue::from_var(self.globals[index as usize].get()))?
                },
                OpCode::GetLocal(index) => {
                    let value = stack.get_mut_value_ptr(index as usize).unwrap();
                    unsafe { stack.push(StackValue::from_var(value))? };
                }
                OpCode::GetField => {
                    let index = stack.pop_scalar_value()?.scalar_as_f64();
                    if index < 0.0 || index > 1024.0 {
                        return Err("invalid field index".to_string());
                    }
                    let index = index as usize;
                    unsafe { stack.push_value((*record.fields[index].get()).share())? };
                }
                OpCode::IndexArrayGetValue => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    let element = array.get_or_insert_uninitialized(key.into())?.share();
                    stack.push_value(element)?
                }
                OpCode::GlobalScalarRef(index) => unsafe {
                    // globals outlive the stack, so this is safe
                    stack.push_ref(self.globals[index as usize].get())?
                },
                OpCode::LocalScalarRef(index) => {
                    let value = stack.get_mut_value_ptr(index as usize).unwrap();
                    unsafe { stack.push_ref(value)? };
                }
                OpCode::FieldRef => {
                    let index = stack.pop_scalar_value()?.scalar_as_f64();
                    if index < 0.0 || index > 1024.0 {
                        return Err("invalid field index".to_string());
                    }
                    let index = index as usize;
                    unsafe { stack.push_ref(record.fields[index].get())? };
                }
                OpCode::IndexArrayGetRef => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    let element_ref =
                        array.get_or_insert_uninitialized(key.into())? as *mut AwkValue;
                    unsafe { stack.push_ref(element_ref)? };
                }
                OpCode::Assign => {
                    let value = stack.pop_scalar_value()?;
                    let lvalue = stack.pop_ref();
                    // FIXME: we don't need to convert values here, just check
                    lvalue.ensure_value_is_scalar()?;
                    fields_state = lvalue.assign(value.clone(), global_env)?;
                    stack.push_value(value)?;
                }
                OpCode::Delete => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    array.delete(&key);
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
                OpCode::Call { id, argc } => {
                    let function = &functions[id as usize];
                    stack.call_function(&function.instructions, argc as usize);
                    ip_increment = 0;
                }
                OpCode::CallBuiltin { function, argc } => match function {
                    BuiltinFunction::Match => {
                        let (start, len) = builtin_match(&mut stack, global_env)?;
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
                            builtin_sprintf(&mut stack, argc - 1, global_env)?
                        } else {
                            print_to_string(&mut stack, argc - 1, global_env)?
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
                            print_to_string(&mut stack, argc - 1, global_env)?
                        } else {
                            builtin_sprintf(&mut stack, argc - 1, global_env)?
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
                    BuiltinFunction::GetLine => {
                        let var = stack.pop_ref();
                        if let Some(next_record) = current_file.read_next_record(&global_env.rs)? {
                            fields_state =
                                var.assign(maybe_numeric_string(next_record), global_env)?;
                            let nr = unsafe { &mut *self.globals[SpecialVar::Nr as usize].get() };
                            nr.assign(global_env.nr as f64 + 1.0, global_env)?;
                            let fnr = unsafe { &mut *self.globals[SpecialVar::Fnr as usize].get() };
                            fnr.assign(global_env.fnr as f64 + 1.0, global_env)?;
                            stack.push_value(1.0)?;
                        } else {
                            stack.push_value(0.0)?;
                        }
                    }
                    BuiltinFunction::GetLineFromFile => {
                        let filename = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        let var = stack.pop_ref();
                        if let Some(next_record) =
                            self.read_files.read_next_record(filename, &global_env.rs)?
                        {
                            fields_state =
                                var.assign(maybe_numeric_string(next_record), global_env)?;
                            stack.push_value(1.0)?;
                        } else {
                            stack.push_value(0.0)?;
                        }
                    }
                    BuiltinFunction::GetLineFromPipe => {
                        let command = stack
                            .pop_scalar_value()?
                            .scalar_to_string(&global_env.convfmt)?;
                        let var = stack.pop_ref();
                        if let Some(next_record) =
                            self.read_pipes.read_next_record(command, &global_env.rs)?
                        {
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
                        self.rand_seed = seed;
                        self.rng = SmallRng::seed_from_u64(self.rand_seed);
                    }
                    other => {
                        fields_state = call_simple_builtin(other, argc, &mut stack, global_env)?
                    }
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
                    let val = stack.pop().unwrap();
                    unsafe {
                        stack
                            .push(val.clone())
                            .expect("failed to push a popped value");
                        stack.push(val)?;
                    }
                }
                OpCode::Pop => {
                    stack.pop();
                }
                OpCode::Next => return Ok(ExecutionResult::Next),
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
                FieldsState::NeedToRecomputeFields => {
                    unsafe { record.recompute_fields(global_env)? };
                    let nf = unsafe { &mut *self.globals[SpecialVar::Nf as usize].get() };
                    nf.assign(record.get_last_field() as f64, global_env)?;
                }
                FieldsState::NeedToRecomputeRecord { changed_field } => {
                    unsafe { record.recompute_record(global_env, changed_field)? };
                    unsafe {
                        *self.globals[SpecialVar::Nf as usize].get() =
                            AwkValue::from(record.get_last_field() as f64);
                    }
                }
            }
            fields_state = FieldsState::Ok;
            stack.ip += ip_increment;
        }
        Ok(ExecutionResult::Expression(
            stack
                .pop()
                .map(|sv| unsafe { sv.into_owned() })
                .unwrap_or(AwkValue::uninitialized()),
        ))
    }

    fn new(args: Array, env: Array, constants: Vec<Constant>, program_globals: usize) -> Self {
        let mut globals = (0..SpecialVar::Count as usize + program_globals)
            .map(|_| AwkValueRef::new(AwkValue::uninitialized()))
            .collect::<Vec<AwkValueRef>>();

        *globals[SpecialVar::Argc as usize].get_mut() = AwkValue::from(args.len() as f64)
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Argc));
        *globals[SpecialVar::Argv as usize].get_mut() =
            AwkValue::from(args).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Argv));
        *globals[SpecialVar::Convfmt as usize].get_mut() = AwkValue::from("%.6g".to_string())
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Convfmt));
        *globals[SpecialVar::Environ as usize].get_mut() =
            AwkValue::from(env).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Environ));
        *globals[SpecialVar::Filename as usize].get_mut() = AwkValue::from("-".to_string())
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Filename));
        *globals[SpecialVar::Fnr as usize].get_mut() =
            AwkValue::from(0.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Fnr));
        *globals[SpecialVar::Fs as usize].get_mut() =
            AwkValue::from(" ").to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Fs));
        *globals[SpecialVar::Nf as usize].get_mut() =
            AwkValue::from(0.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nf));
        *globals[SpecialVar::Nr as usize].get_mut() =
            AwkValue::from(0.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nr));
        *globals[SpecialVar::Ofmt as usize].get_mut() = AwkValue::from("%.6g".to_string())
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ofmt));
        *globals[SpecialVar::Ofs as usize].get_mut() =
            AwkValue::from(" ".to_string()).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ofs));
        *globals[SpecialVar::Ors as usize].get_mut() =
            AwkValue::from("\n".to_string()).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Ors));
        *globals[SpecialVar::Rlength as usize].get_mut() =
            AwkValue::from(0.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rlength));
        *globals[SpecialVar::Rs as usize].get_mut() =
            AwkValue::from("\n".to_string()).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rs));
        *globals[SpecialVar::Rstart as usize].get_mut() =
            AwkValue::from(0.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Rstart));
        *globals[SpecialVar::Subsep as usize].get_mut() = AwkValue::from(" ".to_string())
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Subsep));

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
    assignments: Vec<String>,
) -> Result<(), String> {
    assignments
        .iter()
        .filter_map(|s| parse_assignment(&s))
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
    args: Vec<String>,
    assignments: Vec<String>,
) -> Result<i32, String> {
    // println!("{:?}", program);
    let args = iter::once(("0".to_string(), AwkValue::from("awk")))
        .chain(
            args.into_iter()
                .enumerate()
                .map(|(index, s)| ((index + 1).to_string(), maybe_numeric_string(s).into())),
        )
        .collect();

    let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); STACK_SIZE];
    let mut current_record = Record::default();
    let mut interpreter = Interpreter::new(
        args,
        // TODO: use env args
        Array::default(),
        program.constants,
        program.globals_count,
    );
    let mut global_env = GlobalEnv::default();
    let mut range_pattern_started = vec![false; program.rules.len()];
    let mut return_value = 0;

    set_globals_with_assignment_arguments(
        &mut interpreter,
        &program.globals,
        &mut global_env,
        assignments,
    )?;

    let begin_result = interpreter.run(
        &program.begin_instructions,
        &program.functions,
        &mut current_record,
        &mut stack,
        &mut global_env,
        &mut EmptyRecordReader::default(),
    )?;

    if let ExecutionResult::Exit(val) = begin_result {
        return_value = val;
    }

    let mut current_arg_index = 1;
    'file_loop: loop {
        let argc = interpreter.globals[SpecialVar::Argc as usize]
            .get_mut()
            .scalar_as_f64() as usize;

        if current_arg_index >= argc {
            break;
        }

        let arg = interpreter.globals[SpecialVar::Argv as usize]
            .get_mut()
            .as_array()
            .unwrap()
            .get_or_insert_uninitialized(current_arg_index.to_string().into())
            .unwrap()
            .share()
            .scalar_to_string(&global_env.convfmt)?;

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

        // TODO: check if the arg is an assignment
        global_env.fnr = 1;
        while let Some(record) = reader.read_next_record(&global_env.rs)? {
            current_record.reset(record, &global_env.fs)?;
            interpreter.globals[SpecialVar::Nf as usize].get_mut().value =
                AwkValue::from(current_record.get_last_field() as f64).value;

            interpreter.globals[SpecialVar::Fnr as usize]
                .get_mut()
                .value = AwkValue::from(global_env.fnr as f64).value;
            interpreter.globals[SpecialVar::Nr as usize].get_mut().value =
                AwkValue::from(global_env.nr as f64).value;

            for (i, rule) in program.rules.iter().enumerate() {
                let should_execute = match &rule.pattern {
                    Pattern::All => true,
                    Pattern::Expr(e) => interpreter
                        .run(
                            &e,
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
                                    &end,
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
                                    &start,
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
                        &rule.instructions,
                        &program.functions,
                        &mut current_record,
                        &mut stack,
                        &mut global_env,
                        reader,
                    )?;
                    match rule_result {
                        ExecutionResult::Next => break,
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

    let end_result = interpreter.run(
        &program.end_instructions,
        &program.functions,
        &mut current_record,
        &mut stack,
        &mut global_env,
        &mut EmptyRecordReader::default(),
    )?;
    if let ExecutionResult::Exit(val) = end_result {
        return_value = val;
    }
    Ok(return_value)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::regex::regex_from_str;

    const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

    struct TestResult {
        globals: Vec<AwkValue>,
        execution_result: ExecutionResult,
        record: Record,
    }

    struct Test {
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        functions: Vec<Function>,
        record: Record,
        globals_count: usize,
    }

    impl Test {
        fn new(instructions: Vec<OpCode>, constants: Vec<Constant>) -> Self {
            let globals_count = instructions
                .iter()
                .filter_map(|op| match op {
                    OpCode::GlobalScalarRef(i) | OpCode::GetGlobal(i) => Some(i),
                    _ => None,
                })
                .max()
                .copied()
                .unwrap_or(0)
                .saturating_sub(FIRST_GLOBAL_VAR - 1) as usize;
            Self {
                instructions: instructions.to_vec(),
                constants: constants.to_vec(),
                functions: Default::default(),
                record: Default::default(),
                globals_count,
            }
        }

        fn add_function(mut self, f: Function) -> Self {
            self.functions.push(f);
            self
        }

        fn add_record(mut self, record_string: &str) -> Self {
            self.record
                .reset(record_string.to_string(), &FieldSeparator::Default)
                .expect("could not split record");
            self
        }

        fn run_correct(mut self) -> TestResult {
            let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
            let mut interpreter = Interpreter::new(
                Array::default(),
                Array::default(),
                self.constants,
                self.globals_count,
            );

            let execution_result = interpreter
                .run(
                    &self.instructions,
                    &self.functions,
                    &mut self.record,
                    &mut stack,
                    &mut GlobalEnv::default(),
                    &mut EmptyRecordReader::default(),
                )
                .expect("execution generated an error");

            let globals = interpreter
                .globals
                .into_iter()
                .map(|v| v.into_inner())
                .collect();

            TestResult {
                execution_result,
                globals,
                record: self.record,
            }
        }
    }

    fn interpret_expr(instructions: Vec<OpCode>, constants: Vec<Constant>) -> AwkValue {
        Test::new(instructions, constants)
            .run_correct()
            .execution_result
            .unwrap_expr()
    }

    fn interpret_expr_with_record(
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        record_str: &str,
    ) -> (AwkValue, Record) {
        let result = Test::new(instructions, constants)
            .add_record(record_str)
            .run_correct();
        (result.execution_result.unwrap_expr(), result.record)
    }

    fn test_global(instructions: Vec<OpCode>, constants: Vec<Constant>) -> AwkValue {
        Test::new(instructions, constants).run_correct().globals[FIRST_GLOBAL_VAR as usize].clone()
    }

    fn interpret_with_functions(
        main: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
        functions: Vec<Function>,
    ) -> AwkValue {
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
        let mut interpreter =
            Interpreter::new(Array::default(), Array::default(), constants, global_count);
        interpreter
            .run(
                &main,
                &functions,
                &mut Record::default(),
                &mut stack,
                &mut GlobalEnv::default(),
                &mut EmptyRecordReader::default(),
            )
            .expect("error running test")
            .unwrap_expr()
    }

    fn test_sprintf(format: &str, args: Vec<Constant>) -> String {
        let mut instructions = vec![OpCode::PushConstant(0)];
        let mut constants = vec![Constant::from(format)];
        let argc = args.len() + 1;
        for (i, c) in args.into_iter().enumerate() {
            instructions.push(OpCode::PushConstant(i as u32 + 1));
            constants.push(c);
        }
        instructions.push(OpCode::CallBuiltin {
            function: BuiltinFunction::Sprintf,
            argc: argc as u16,
        });
        let result = interpret_expr(instructions, constants);
        if let AwkValueVariant::String(s) = result.value {
            s.to_string()
        } else {
            panic!("expected string, got {:?}", result);
        }
    }

    #[test]
    fn test_push_constant() {
        let instructions = vec![OpCode::PushConstant(0)];
        let constant = vec![Constant::Number(1.0)];
        assert_eq!(
            interpret_expr(instructions.clone(), constant),
            AwkValue::from(1.0)
        );

        let constant = vec![Constant::from("hello")];
        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from("hello".to_string())
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
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
    }

    #[test]
    fn test_sub() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Sub,
        ];
        let constant = vec![Constant::Number(145.0), Constant::Number(123.0)];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(22.0));
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
            interpret_expr(instructions, constant),
            AwkValue::from(144.0)
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

        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(12.0));
    }

    #[test]
    fn test_mod() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Mod,
        ];
        let constant = vec![Constant::Number(144.0), Constant::Number(12.0)];

        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
    }

    #[test]
    fn test_pow() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Pow,
        ];
        let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];

        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(8.0));
    }

    #[test]
    fn test_string_concat() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Concat,
        ];
        let constant = vec![Constant::from("hello"), Constant::from("world")];

        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from("helloworld".to_string())
        );
    }

    #[test]
    fn test_compare_same_operand_type() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));

        let constant = vec![Constant::from("abcd"), Constant::from("efgh")];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
    }

    #[test]
    fn test_compare_number_string() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::from("2.")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));

        let constant = vec![Constant::from("abcd"), Constant::Number(3.0)];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    fn test_compare_number_uninitialized() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::Ge,
        ];
        let constant = vec![Constant::Number(2.0)];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    fn test_interpret_in_for_global_array() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::from("key")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));

        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::from("key")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    fn test_interpret_in_for_local_array_ref() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::from("key")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));

        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::from("key")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    fn test_negate() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Negate];
        let constant = vec![Constant::Number(456.0)];
        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from(-456.0)
        );
    }

    #[test]
    fn test_not() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Not];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    fn test_postinc() {
        let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostInc];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
    }

    #[test]
    fn test_postdec() {
        let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PostDec];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
    }

    #[test]
    fn test_preinc() {
        let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreInc];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
    }

    #[test]
    fn test_predec() {
        let instructions = vec![OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR), OpCode::PreDec];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
    }

    #[test]
    fn test_assign_to_global_var() {
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constant = vec![Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), AwkValue::from(123.0));
    }

    #[test]
    fn test_assign_to_array_element() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::from("key"), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            Array::from_iter([("key", 123.0)]).into()
        );
    }

    #[test]
    fn test_delete_global_array_element_after_insertion() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::from("key"), Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_delete_global_array_element_after_insertion_through_local_ref() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::from("key"), Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_delete_from_empty_global_array() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::from("key")];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_assign_to_local_var() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::LocalScalarRef(0),
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::Number(0.0), Constant::from("test string")];
        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from("test string".to_string())
        );
    }

    #[test]
    fn test_assign_to_array_through_local_ref() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::GetLocal(0),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::from("key"), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            Array::from_iter([("key", 123.0)]).into()
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
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
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
            Constant::from(""),
            Constant::Number(1.0),
            Constant::Number(2.0),
        ];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(2.0));
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
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(3.0));
    }

    #[test]
    fn test_call_function_without_args() {
        let main = vec![OpCode::Call { id: 0, argc: 0 }];
        let functions = vec![Function {
            parameters_count: 0,
            instructions: vec![OpCode::PushConstant(0), OpCode::Return],
        }];
        let constant = vec![Constant::from("test")];
        assert_eq!(
            interpret_with_functions(main, constant, 0, functions),
            AwkValue::from("test".to_string())
        );
    }

    #[test]
    fn test_call_with_uninitialized_scalar_argument() {
        let main = vec![OpCode::PushUninitialized, OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![OpCode::LocalScalarRef(0), OpCode::Return],
        }];
        assert_eq!(
            interpret_with_functions(main, vec![], 1, functions),
            AwkValue::uninitialized_scalar()
        );
    }

    #[test]
    fn test_call_with_uninitialized_array_argument() {
        let main = vec![OpCode::PushUninitialized, OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetValue,
                OpCode::Return,
            ],
        }];
        let constant = vec![Constant::from("key")];
        assert_eq!(
            interpret_with_functions(main, constant, 1, functions),
            AwkValue::uninitialized_scalar()
        );
    }

    #[test]
    fn test_call_function_with_scalar_argument() {
        let main = vec![OpCode::PushConstant(0), OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![OpCode::GetLocal(0), OpCode::PushOne, OpCode::Add],
        }];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(
            interpret_with_functions(main, constant, 0, functions),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_call_function_with_array_argument() {
        let main = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::Call { id: 0, argc: 1 },
        ];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetRef,
                OpCode::PushOne,
                OpCode::Assign,
            ],
        }];
        let constants = vec![Constant::from("key")];
        assert_eq!(
            interpret_with_functions(main, constants, 1, functions),
            AwkValue::from(1.0)
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
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::GetLocal(2),
                OpCode::GetLocal(3),
                OpCode::GetLocal(4),
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
            AwkValue::from(5.0)
        );
    }

    #[test]
    fn test_access_whole_record_field() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::FieldRef];
        let constant = vec![Constant::Number(0.0)];
        let value = Test::new(instructions, constant)
            .add_record("hello")
            .run_correct()
            .execution_result
            .unwrap_expr();
        assert_eq!(value, "hello".into());
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

        let result = Test::new(instructions, constants)
            .add_record("1")
            .run_correct();

        assert_eq!(result.globals[SpecialVar::Nf as usize], AwkValue::from(9.0));
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
        let constant = vec![Constant::from("hello"), Constant::from("l")];
        assert_eq!(
            interpret_expr(instructions.clone(), constant),
            AwkValue::from(3.0)
        );

        let constant = vec![Constant::from("hello"), Constant::from("z")];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(0.0));
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
        let constant = vec![Constant::from("hello")];
        assert_eq!(
            interpret_expr(instructions, constant.clone()),
            AwkValue::from(5.0)
        );

        let instructions = vec![
            OpCode::PushZero,
            OpCode::GetField,
            OpCode::CallBuiltin {
                function: BuiltinFunction::Length,
                argc: 1,
            },
        ];
        let value = Test::new(instructions, constant)
            .add_record("test record")
            .run_correct()
            .execution_result
            .unwrap_expr();
        assert_eq!(value, AwkValue::from(11.0));
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
            Constant::from("hello"),
            Constant::Number(2.0),
            Constant::Number(2.0),
        ];
        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from("el".to_string())
        );

        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Substr,
                argc: 2,
            },
        ];
        let constant = vec![Constant::from("hello"), Constant::Number(3.0)];
        assert_eq!(
            interpret_expr(instructions, constant),
            AwkValue::from("llo".to_string())
        );
    }

    #[test]
    fn test_builtin_sprintf_with_escaped_percent_sign() {
        assert_eq!(test_sprintf("%%", vec![]), "%");
    }

    #[test]
    fn test_builtin_sprintf_with_string_args() {
        assert_eq!(test_sprintf("hello", vec![]), "hello");
        assert_eq!(
            test_sprintf("hello %s", vec![Constant::from("world")]),
            "hello world"
        );
        assert_eq!(
            test_sprintf(
                "%s:%s:%s",
                vec![
                    Constant::from("a"),
                    Constant::from("b"),
                    Constant::from("c")
                ]
            ),
            "a:b:c"
        );
        assert_eq!(
            test_sprintf("%10s", vec![Constant::from("test")]),
            "      test"
        );
        assert_eq!(
            test_sprintf("%-10s", vec![Constant::from("test")]),
            "test      "
        );
        assert_eq!(test_sprintf("%.2s", vec![Constant::from("test")]), "te");
        assert_eq!(test_sprintf("%.20s", vec![Constant::from("test")]), "test");
        assert_eq!(
            test_sprintf("%10.2s", vec![Constant::from("test")]),
            "        te"
        );
        assert_eq!(
            test_sprintf("%-10.2s", vec![Constant::from("test")]),
            "te        "
        );
        assert_eq!(
            test_sprintf("%10.20s", vec![Constant::from("test")]),
            "      test"
        );
    }

    #[test]
    fn test_builtin_sprintf_with_float_args_hex_format() {
        assert_eq!(
            test_sprintf("%a", vec![Constant::Number(255.34)]),
            "0x1.feae147ae147bp+7"
        );
        assert_eq!(
            test_sprintf("%.5a", vec![Constant::Number(255.34)]),
            "0x1.feae1p+7"
        );
        assert_eq!(
            test_sprintf("%35a", vec![Constant::Number(255.34)]),
            "               0x1.feae147ae147bp+7"
        );
        assert_eq!(
            test_sprintf("%-35a", vec![Constant::Number(255.34)]),
            "0x1.feae147ae147bp+7               "
        );
        assert_eq!(
            test_sprintf("float value: %a", vec![Constant::Number(49.67)]),
            "float value: 0x1.8d5c28f5c28f6p+5"
        );
        assert_eq!(
            test_sprintf("%10.3a", vec![Constant::Number(255.34)]),
            "0x1.feap+7"
        );
    }

    #[test]
    fn test_builtin_sprintf_with_float_args() {
        assert_eq!(
            test_sprintf("%f", vec![Constant::Number(255.34)]),
            "255.340000"
        );
        assert_eq!(
            test_sprintf("%.1f", vec![Constant::Number(255.34)]),
            "255.3"
        );
        assert_eq!(
            test_sprintf("%15f", vec![Constant::Number(255.34)]),
            "     255.340000"
        );
        assert_eq!(
            test_sprintf("%-15f", vec![Constant::Number(255.34)]),
            "255.340000     "
        );
        assert_eq!(
            test_sprintf("float value: %f", vec![Constant::Number(49.67)]),
            "float value: 49.670000"
        );
        assert_eq!(
            test_sprintf("%10.3f", vec![Constant::Number(255.34)]),
            "   255.340"
        );
    }

    #[test]
    fn test_builtin_sprintf_scientific_float() {
        assert_eq!(
            test_sprintf("%e", vec![Constant::Number(255.34)]),
            "2.553400e+02"
        );
        assert_eq!(
            test_sprintf("%.1e", vec![Constant::Number(255.34)]),
            "2.6e+02"
        );
        assert_eq!(
            test_sprintf("%15e", vec![Constant::Number(255.34)]),
            "   2.553400e+02"
        );
        assert_eq!(
            test_sprintf("%-15e", vec![Constant::Number(255.34)]),
            "2.553400e+02   "
        );
        assert_eq!(
            test_sprintf("float value: %e", vec![Constant::Number(49.67)]),
            "float value: 4.967000e+01"
        );
        assert_eq!(
            test_sprintf("%10.3e", vec![Constant::Number(255.34)]),
            " 2.553e+02"
        );
    }

    #[test]
    fn test_builtin_sprintf_float_general() {
        assert_eq!(test_sprintf("%g", vec![Constant::Number(255.34)]), "255.34");
        assert_eq!(
            test_sprintf("%.1g", vec![Constant::Number(255.34)]),
            "3e+02"
        );
        assert_eq!(
            test_sprintf("%15g", vec![Constant::Number(255.34)]),
            "         255.34"
        );
        assert_eq!(
            test_sprintf("%-15g", vec![Constant::Number(255.34)]),
            "255.34         "
        );
        assert_eq!(
            test_sprintf("float value: %g", vec![Constant::Number(49.67)]),
            "float value: 49.67"
        );
        assert_eq!(
            test_sprintf("%10.3g", vec![Constant::Number(255.34)]),
            "       255"
        );
    }

    #[test]
    fn test_builtin_sprintf_char() {
        assert_eq!(test_sprintf("%c", vec![Constant::Number(55.0)]), "7");
        assert_eq!(test_sprintf("%c", vec![Constant::Number(548.0)]), "$");
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_match_op() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Match,
        ];
        let constant = vec![
            Constant::from("hello"),
            Constant::Regex(Rc::new(
                Regex::new(CString::new("e").unwrap()).expect("failed to compile regex"),
            )),
        ];
        assert_eq!(interpret_expr(instructions, constant), AwkValue::from(1.0));
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_match() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Match,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::from("this is a test"),
            Constant::Regex(Rc::new(regex_from_str("is* a"))),
        ];
        let result = Test::new(instructions, constants).run_correct();

        assert_eq!(result.execution_result.unwrap_expr(), AwkValue::from(6.0));
        assert_eq!(
            result.globals[SpecialVar::Rstart as usize],
            AwkValue::from(6.0)
        );
        assert_eq!(
            result.globals[SpecialVar::Rlength as usize],
            AwkValue::from(4.0)
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_split_with_split_ere() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Split,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::from("a, b, c"),
            Constant::Regex(Rc::new(regex_from_str(","))),
        ];

        let global = test_global(instructions, constants);
        assert_eq!(
            global,
            Array::from_iter([("1", "a"), ("2", " b"), ("3", " c")]).into()
        );
    }

    #[test]
    fn test_builtin_split_with_default_fs() {
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Split,
                argc: 2,
            },
        ];
        let constants = vec![Constant::from("a b c")];

        let global = test_global(instructions, constants);
        assert_eq!(
            global,
            Array::from_iter([("1", "a"), ("2", "b"), ("3", "c")]).into()
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_sub() {
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Sub,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::from("aaabbbabaabb"),
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::from("x"),
        ];

        let global = test_global(instructions, constants);
        assert_eq!(global, "aaxabaabb".into());
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_sub_on_record() {
        let instructions = vec![
            OpCode::PushZero,
            OpCode::FieldRef,
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Sub,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::from("x"),
        ];
        let mut record = Test::new(instructions, constants)
            .add_record("aaabbb ab aabb")
            .run_correct()
            .record;
        assert_eq!(
            *record.fields[0].get_mut(),
            AwkValue::field_ref("aax ab aabb", 0)
        );
        assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
        assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("ab", 2));
        assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("aabb", 3));
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_gsub() {
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Gsub,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::from("aaabbbabaabb"),
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::from("x"),
        ];

        let global = test_global(instructions, constants);
        assert_eq!(global, "aaxxax".into());
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_builtin_gsub_on_record() {
        let instructions = vec![
            OpCode::PushZero,
            OpCode::FieldRef,
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Gsub,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::from("x"),
        ];
        let mut record = Test::new(instructions, constants)
            .add_record("aaabbb ab aabb")
            .run_correct()
            .record;
        assert_eq!(
            *record.fields[0].get_mut(),
            AwkValue::field_ref("aax x ax", 0)
        );
        assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
        assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("x", 2));
        assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("ax", 3));
    }

    #[test]
    fn test_iterate_through_empty_global_array() {
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
            OpCode::CreateGlobalIterator(FIRST_GLOBAL_VAR),
            OpCode::AdvanceIterOrJump(2),
            OpCode::Jump(-1),
        ];
        let result = Test::new(instructions, vec![]).run_correct();
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize],
            Array::default().into()
        );
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize + 1],
            AwkValue::uninitialized_scalar()
        );
    }

    #[test]
    fn test_iterate_through_global_array() {
        // ```
        // a["key1"] = "value"
        // a["key2"] = "value"
        // a["key3"] = "value"
        // for (a in array) {}
        //```
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(2),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(3),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
            OpCode::CreateGlobalIterator(FIRST_GLOBAL_VAR),
            OpCode::AdvanceIterOrJump(2),
            OpCode::Jump(-1),
        ];
        let constants = vec![
            Constant::from("value"),
            Constant::from("key1"),
            Constant::from("key2"),
            Constant::from("key3"),
        ];
        let result = Test::new(instructions, constants).run_correct();
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize],
            Array::from_iter([("key1", "value"), ("key2", "value"), ("key3", "value")]).into()
        );
        assert_eq!(result.globals[FIRST_GLOBAL_VAR as usize + 1], "key3".into());
    }

    #[test]
    fn test_iterate_through_empty_local_array() {
        let instructions = vec![
            OpCode::PushUninitialized,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::CreateLocalIterator(0),
            OpCode::AdvanceIterOrJump(2),
            OpCode::Jump(-1),
        ];
        let result = Test::new(instructions, vec![]).run_correct();
        assert_eq!(
            result.execution_result.unwrap_expr(),
            Array::default().into()
        );
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize],
            AwkValue::uninitialized_scalar()
        );
    }

    #[test]
    fn test_iterate_through_local_array() {
        // ```
        // a["key1"] = "value"
        // a["key2"] = "value"
        // a["key3"] = "value"
        // for (a in array) {}
        //```
        let instructions = vec![
            OpCode::PushUninitialized,
            OpCode::LocalScalarRef(0),
            OpCode::PushConstant(1),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::LocalScalarRef(0),
            OpCode::PushConstant(2),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::LocalScalarRef(0),
            OpCode::PushConstant(3),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::CreateLocalIterator(0),
            OpCode::AdvanceIterOrJump(2),
            OpCode::Jump(-1),
        ];
        let constants = vec![
            Constant::from("value"),
            Constant::from("key1"),
            Constant::from("key2"),
            Constant::from("key3"),
        ];
        let result = Test::new(instructions, constants).run_correct();
        assert_eq!(
            result.execution_result.unwrap_expr(),
            Array::from_iter([("key1", "value"), ("key2", "value"), ("key3", "value")]).into()
        );
        assert_eq!(result.globals[FIRST_GLOBAL_VAR as usize], "key3".into());
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_ere_outside_match_matches_record() {
        let instructions = vec![OpCode::PushConstant(0)];
        let constants = vec![Constant::Regex(Rc::new(regex_from_str("test")))];
        let result = Test::new(instructions, constants)
            .add_record("this is a test")
            .run_correct();
        let value = result.execution_result.unwrap_expr();
        assert_eq!(value.scalar_as_f64(), 1.0);
        assert_eq!(value.scalar_to_string("").unwrap(), "1".to_string().into());
    }

    #[test]
    fn test_scalars_are_passed_by_value() {
        // ```
        // a = "test value"
        // b = "test value"
        // f(a, b)
        // ```
        let instructions = vec![
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalScalarRef(FIRST_GLOBAL_VAR + 1),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::GetGlobal(FIRST_GLOBAL_VAR + 1),
            OpCode::Call { id: 0, argc: 2 },
        ];
        // ```
        // function f(a, b) {
        //  a = 1;
        //  b = 1;
        // }
        // ```
        let function = Function {
            parameters_count: 2,
            instructions: vec![
                OpCode::LocalScalarRef(0),
                OpCode::PushOne,
                OpCode::Assign,
                OpCode::Pop,
                OpCode::LocalScalarRef(1),
                OpCode::PushOne,
                OpCode::Assign,
                OpCode::Pop,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ],
        };
        let constants = vec!["test value".into()];
        let result = Test::new(instructions, constants)
            .add_function(function)
            .run_correct();
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize],
            "test value".into()
        );
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize + 1],
            "test value".into()
        );
    }

    #[test]
    fn test_arrays_are_passed_by_reference() {
        // ```
        // a["key"] = "value"
        // f(a)
        // ```
        let instructions = vec![
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArrayGetRef,
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GetGlobal(FIRST_GLOBAL_VAR),
            OpCode::Call { id: 0, argc: 1 },
        ];
        // ```
        // function f(a) {
        //  a["key"] = "new value"
        // }
        // ```
        let function = Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::GetLocal(0),
                OpCode::PushConstant(0),
                OpCode::IndexArrayGetRef,
                OpCode::PushConstant(2),
                OpCode::Assign,
                OpCode::PushUninitializedScalar,
                OpCode::Return,
            ],
        };
        let constants = vec![
            Constant::from("key"),
            Constant::from("value"),
            Constant::from("new value"),
        ];
        let result = Test::new(instructions, constants)
            .add_function(function)
            .run_correct();
        assert_eq!(
            result.globals[FIRST_GLOBAL_VAR as usize],
            Array::from_iter([("key", "new value")]).into()
        );
    }

    #[test]
    fn test_changing_a_field_recomputes_the_record() {
        let instructions = vec![
            OpCode::PushOne,
            OpCode::FieldRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constants = vec![Constant::from("test")];

        let mut record = Test::new(instructions, constants)
            .add_record("a b")
            .run_correct()
            .record;
        assert_eq!(
            *record.fields[0].get_mut(),
            AwkValue::field_ref("test b", 0)
        );
        assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("test", 1));
        assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("b", 2));
    }

    #[test]
    fn test_changing_the_record_recomputes_fields() {
        let instructions = vec![
            OpCode::PushZero,
            OpCode::FieldRef,
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constants = vec![Constant::from("test")];

        let mut result = Test::new(instructions, constants)
            .add_record("a b")
            .run_correct();
        assert_eq!(*result.record.last_field.get_mut(), 1);
        assert_eq!(
            *result.record.fields[0].get_mut(),
            AwkValue::field_ref("test", 0)
        );
        assert_eq!(
            *result.record.fields[1].get_mut(),
            AwkValue::field_ref("test", 1)
        );
        assert_eq!(
            result.globals[SpecialVar::Nf as usize],
            AwkValue::from(1.0).to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Nf))
        );
    }
}
