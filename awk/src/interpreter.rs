//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::format::{
    fmt_write_decimal_float, fmt_write_float_general, fmt_write_hex_float,
    fmt_write_scientific_float, fmt_write_signed, fmt_write_string, fmt_write_unsigned,
    parse_conversion_specifier_args, FormatArgs, IntegerFormat,
};
use crate::program::{BuiltinFunction, Constant, Function, OpCode, Pattern, Program, SpecialVar};
use crate::regex::Regex;
use std::cell::UnsafeCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::Write;
use std::marker::PhantomData;
use std::rc::Rc;
use std::{iter, u16};

const STACK_SIZE: usize = 2048;

fn bool_to_f64(p: bool) -> f64 {
    if p {
        1.0
    } else {
        0.0
    }
}

fn strtod(s: &str) -> f64 {
    lexical::parse_partial_with_options::<f64, _, { lexical::format::C_LITERAL }>(
        s,
        &lexical::ParseFloatOptions::default(),
    )
    .map(|(val, _)| val)
    .unwrap_or(0.0)
}

fn is_integer(num: f64) -> bool {
    num.is_finite() && num.fract() == 0.0
}

fn swap_with_default<T: Default>(value: &mut T) -> T {
    let mut result = T::default();
    std::mem::swap(&mut result, value);
    result
}

fn sprintf(
    format_string: &str,
    values: &mut [AwkValue],
    float_format: &str,
) -> Result<String, String> {
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
                        let value = value.scalar_to_string(float_format)?;
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
    Ok(result)
}

fn builtin_match(stack: &mut Stack, global_env: &mut GlobalEnv) -> Result<(f64, f64), String> {
    let ere = stack.pop_value().to_ere()?;
    let string = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    // TODO: should look into this unwrap
    let mut locations = ere.match_locations(CString::new(string).unwrap());
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

fn gsub(ere: &Regex, repl: &str, in_str: &str, only_replace_first: bool) -> (String, usize) {
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
    (result, num_replacements)
}

fn builtin_gsub(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
    is_sub: bool,
) -> Result<FieldsState, String> {
    // FIXME: this is not safe. in_str could be a reference to either repl or ere
    let in_str = stack.pop().unwrap().unwrap_ptr();
    let repl = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    let ere = stack.pop_value().to_ere()?;
    let in_str = unsafe { &mut *in_str };
    in_str.ensure_value_is_scalar()?;
    let (result, count) = gsub(
        &ere,
        &repl,
        &in_str.to_owned().scalar_to_string(&global_env.convfmt)?,
        is_sub,
    );
    stack.push_value(count as f64)?;
    in_str.assign(result, global_env)
}

fn call_simple_builtin(
    function: BuiltinFunction,
    argc: u16,
    stack: &mut Stack,
    record: &CString,
    global_env: &mut GlobalEnv,
) -> Result<(), String> {
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
        BuiltinFunction::Rand => {
            todo!()
        }
        BuiltinFunction::Srand => {
            todo!()
        }
        BuiltinFunction::Index => {
            let t = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let index = s.find(&t).map(|i| i as f64 + 1.0).unwrap_or(0.0);
            stack.push_value(index)?;
        }
        BuiltinFunction::Length => {
            if argc == 0 {
                stack.push_value(record.count_bytes() as f64)?;
            } else {
                let value = stack
                    .pop_scalar_value()?
                    .scalar_to_string(&global_env.convfmt)?;
                stack.push_value(value.len() as f64)?;
            }
        }
        BuiltinFunction::Split => {
            let separator = if argc == 2 {
                None
            } else {
                Some(FieldSeparator::Ere(stack.pop_value().to_ere()?))
            };
            // FIXME: check above that array_ref is a StackValue::ValueRef and that it
            // is an array. If this is true, `array_ref` cannot point to `s` since it is
            // a string
            let array_ref = stack.pop().unwrap();
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            // this is safe only if the value in `array_ref` is not a reference to
            // s, which we just popped
            let array = unsafe { (*array_ref.unwrap_ptr()).as_array()? };
            array.clear();

            split_record(
                &s,
                &separator.iter().next().unwrap_or(&global_env.fs),
                |i, s| array.set((i + 1).to_string(), s),
            );
            let n = array.len();
            stack.push_value(n as f64)?;
        }
        BuiltinFunction::Sprintf => {
            let mut values = Vec::new();
            for _ in 0..argc - 1 {
                values.push(stack.pop_scalar_value()?);
            }
            let format_string = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let result = sprintf(&format_string, &mut values, &global_env.convfmt)?;
            stack.push_value(result)?;
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
            print!("{}{}", output, global_env.ors);
        }
        BuiltinFunction::Printf => {
            todo!()
        }
        _ => unreachable!("call_simple_builtin was passed an invalid builtin function kind"),
    }
    Ok(())
}

enum FieldSeparator {
    Default,
    Char(u8),
    Ere(Rc<Regex>),
}

fn split_record<S: FnMut(usize, &str)>(
    record: &str,
    field_separator: &FieldSeparator,
    mut store_result: S,
) {
    match field_separator {
        FieldSeparator::Default => record
            .trim_start()
            .split_ascii_whitespace()
            .enumerate()
            .for_each(|(i, s)| store_result(i, s)),
        FieldSeparator::Char(c) => record
            .split(*c as char)
            .enumerate()
            .for_each(|(i, s)| store_result(i, s)),
        FieldSeparator::Ere(re) => {
            let mut split_start = 0;
            let mut index = 0;
            let mut iter = re.match_locations(CString::new(record).unwrap());
            while let Some(separator_range) = iter.next() {
                store_result(index, &record[split_start..separator_range.start]);
                split_start = separator_range.end;
                index += 1;
            }
            store_result(index, &record[split_start..])
        }
    }
}

impl TryFrom<String> for FieldSeparator {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value == " " {
            Ok(FieldSeparator::Default)
        } else if value.len() == 1 {
            Ok(FieldSeparator::Char(*value.as_bytes().first().unwrap()))
        } else {
            let c_str = CString::new(value).map_err(|_| "invalid string".to_string())?;
            let ere = Regex::new(c_str)?;
            Ok(FieldSeparator::Ere(Rc::from(ere)))
        }
    }
}

enum RecordSeparator {
    Char(char),
    Null,
}

fn first_record(buffer: &str, separator: &RecordSeparator) -> (String, usize) {
    match separator {
        RecordSeparator::Char(sep) => {
            let str = buffer
                .chars()
                .take_while(|c| *c != *sep)
                .collect::<String>();
            let chars_read = str.len() + 1;
            (str, chars_read)
        }
        RecordSeparator::Null => {
            let leading_whitespace_bytes = buffer
                .chars()
                .take_while(|c| c.is_whitespace())
                .map(|c| c.len_utf8())
                .sum();
            let str = buffer[leading_whitespace_bytes..]
                .chars()
                .take_while(|c| *c != '\n')
                .collect::<String>();
            let trailing_whitespace_bytes: usize = buffer[leading_whitespace_bytes + str.len()..]
                .chars()
                .take_while(|c| c.is_whitespace())
                .map(|c| c.len_utf8())
                .sum();
            let chars_read = leading_whitespace_bytes + str.len() + trailing_whitespace_bytes;
            (str, chars_read)
        }
    }
}

impl TryFrom<String> for RecordSeparator {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let mut iter = value.chars();
        let result = match iter.next() {
            Some(c) => RecordSeparator::Char(c),
            None => RecordSeparator::Null,
        };
        if iter.next().is_some() {
            Err("the record separator cannot contain more than one characters".to_string())
        } else {
            Ok(result)
        }
    }
}

struct GlobalEnv {
    convfmt: String,
    fs: FieldSeparator,
    ofs: String,
    ors: String,
    ofmt: String,
    rs: RecordSeparator,
}

impl GlobalEnv {
    fn set(&mut self, var: SpecialVar, value: &AwkValue) -> Result<(), String> {
        let string_val = value.to_owned().scalar_to_string(&self.convfmt)?;
        match var {
            SpecialVar::Convfmt => self.convfmt = string_val,
            SpecialVar::Fs => self.fs = string_val.try_into()?,
            SpecialVar::Ofmt => self.ofmt = string_val,
            SpecialVar::Ofs => self.ofs = string_val,
            SpecialVar::Ors => self.ors = string_val,
            SpecialVar::Rs => self.rs = string_val.try_into()?,
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
            convfmt: String::from("%.6g"),
            fs: FieldSeparator::Default,
            ofs: String::from(" "),
            ors: String::from("\n"),
            ofmt: String::from("%.6g"),
            rs: RecordSeparator::Char('\n'),
        }
    }
}

enum FieldsState {
    NeedToRecomputeFields,
    NeedToRecomputeRecord { changed_field: usize },
    Ok,
}

struct Record {
    record: CString,
    fields: Vec<AwkValueRef>,
    last_field: usize,
}

impl Record {
    fn from_string(record: String, field_separator: &FieldSeparator) -> Result<Self, String> {
        let mut r = Self::default();
        r.reset(record, field_separator).map(|_| r)
    }

    fn reset(&mut self, record: String, field_separator: &FieldSeparator) -> Result<(), String> {
        let previous_last_field = self.last_field;
        self.last_field = 0;
        split_record(&record, field_separator, |i, s| {
            let field_index = i + 1;
            self.last_field += 1;
            *self.fields[field_index].get_mut() = AwkValue::field_ref(s, field_index as u16);
        });
        *self.fields[0].get_mut() = AwkValue::field_ref(record.clone(), 0);
        self.record = CString::new(record).map_err(|_| "invalid string".to_string())?;
        if self.last_field < previous_last_field {
            for field in &mut self.fields[self.last_field..=previous_last_field] {
                field.get_mut().value = AwkValueVariant::UninitializedScalar;
            }
        }
        Ok(())
    }
}

impl Default for Record {
    fn default() -> Self {
        let fields = (0..1024)
            .map(|i| AwkValueRef::new(AwkValue::field_ref(AwkValue::uninitialized_scalar(), i)))
            .collect();
        Self {
            record: CString::default(),
            // TODO: fix magic number
            fields,
            last_field: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ArrayElement {
    value: AwkValue,
    key_index: usize,
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Array {
    values: HashMap<Rc<str>, ArrayElement>,
    keys: Vec<Rc<str>>,
}

impl Array {
    fn delete(&mut self, key: &str) {
        if let Some(array_element) = self.values.remove(key) {
            self.keys.swap_remove(array_element.key_index);
            if self.keys.len() > 0 {
                let swapped_key = self.keys[array_element.key_index].as_ref();
                self.values.get_mut(swapped_key).unwrap().key_index = array_element.key_index;
            }
        }
    }

    fn get_or_insert_uninitialized(&mut self, key: String) -> &mut AwkValue {
        let key = Rc::<str>::from(key);
        &mut self
            .values
            .entry(key.clone())
            .or_insert_with(|| {
                let key_index = self.keys.len();
                self.keys.push(key);
                ArrayElement {
                    value: AwkValue::uninitialized_scalar(),
                    key_index,
                }
            })
            .value
    }

    fn set<V: Into<AwkValue>>(&mut self, key: String, value: V) {
        let key = Rc::<str>::from(key);
        let value = value.into();
        match self.values.entry(key.clone()) {
            Entry::Occupied(e) => e.into_mut().value = value,
            Entry::Vacant(e) => {
                let key_index = self.keys.len();
                self.keys.push(key.clone());
                e.insert(ArrayElement { value, key_index });
            }
        }
    }

    fn contains(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }

    fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    fn len(&self) -> usize {
        assert_eq!(self.values.len(), self.keys.len());

        self.values.len()
    }
}

impl<S: Into<String>, A: Into<AwkValue>> FromIterator<(S, A)> for Array {
    fn from_iter<T: IntoIterator<Item = (S, A)>>(iter: T) -> Self {
        let mut result = Self::default();
        for (key, val) in iter {
            result.set(key.into(), val);
        }
        result
    }
}

#[derive(Debug, Clone, PartialEq)]
enum AwkValueVariant {
    Number(f64),
    String(String),
    Array(Array),
    Regex {
        ere: Rc<Regex>,
        matches_record: bool,
    },
    Uninitialized,
    UninitializedScalar,
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
            AwkValueVariant::String(s) => strtod(s),
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

    fn scalar_to_string(self, num_fmt: &str) -> Result<String, String> {
        match self.value {
            AwkValueVariant::Number(num) => {
                if is_integer(num) {
                    Ok((num as i64).to_string())
                } else {
                    sprintf(num_fmt, &mut [num.into()], num_fmt)
                }
            }
            AwkValueVariant::String(s) => Ok(s),
            AwkValueVariant::UninitializedScalar => Ok(String::new()),
            AwkValueVariant::Array(_)
            | AwkValueVariant::Regex { .. }
            | AwkValueVariant::Uninitialized => {
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
            AwkRefType::SpecialGlobalVar(special_var) => global_env.set(special_var, &self)?,
            AwkRefType::Field(index) => {
                return if index == 0 {
                    Ok(FieldsState::NeedToRecomputeFields)
                } else {
                    Ok(FieldsState::NeedToRecomputeRecord {
                        changed_field: index as usize,
                    })
                }
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

    fn to_owned(&self) -> Self {
        Self {
            value: self.value.clone(),
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

    fn numeric_string(s: String) -> Self {
        Self {
            value: AwkValueVariant::String(s),
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
        value.into().to_ref(AwkRefType::Field(field_index))
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

impl From<String> for AwkValue {
    fn from(value: String) -> Self {
        let value = value.into();
        Self {
            value: AwkValueVariant::String(value),
            ref_type: AwkRefType::None,
        }
    }
}

impl From<&str> for AwkValue {
    fn from(value: &str) -> Self {
        value.to_string().into()
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
enum StackValue {
    Value(AwkValue),
    ValueRef(*mut AwkValue),
}

impl StackValue {
    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn value_ref(&mut self) -> &mut AwkValue {
        match self {
            StackValue::Value(val) => val,
            StackValue::ValueRef(val_ref) => &mut **val_ref,
        }
    }

    fn unwrap_ptr(self) -> *mut AwkValue {
        match self {
            StackValue::ValueRef(ptr) => ptr,
            _ => unreachable!("expected lvalue"),
        }
    }

    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn to_owned(self) -> AwkValue {
        match self {
            StackValue::Value(val) => val,
            StackValue::ValueRef(ref_val) => (*ref_val).to_owned(),
        }
    }

    /// # Safety
    /// if the `StackValue` is a `ValueRef` then the pointer has to point to a valid `AwkValue`
    unsafe fn ensure_value_is_scalar(&mut self) -> Result<(), String> {
        self.value_ref().ensure_value_is_scalar()
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
            Ok(value.to_owned())
        }
    }

    fn pop_value(&mut self) -> AwkValue {
        // safe by type invariance
        unsafe {
            let value = self.pop().unwrap();
            value.to_owned()
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

    /// Pushes a reference to a value on the stack. If at the given index there is already
    /// a `StackValue::ValueRef`, it just clones it and pushes it on the stack
    /// # Panics
    /// panics if `value_index` is out of bounds
    fn push_ref_to_stack_element(&mut self, value_index: usize) -> Result<(), String> {
        assert!(unsafe { self.sp.offset_from(self.bp) } >= value_index as isize);

        let value = unsafe { &mut *self.bp.add(value_index) };
        match value {
            StackValue::Value(val) => unsafe { self.push_ref(val) },
            StackValue::ValueRef(val_ref) => unsafe { self.push_ref(*val_ref) },
        }
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

struct Interpreter {
    globals: Vec<AwkValueRef>,
    constants: Vec<Constant>,
    convfmt: String,
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
                $stack.push_value(bool_to_f64(lhs $op rhs))?;
            }
            (AwkValueVariant::Number(lhs), AwkValueVariant::UninitializedScalar) => {
                $stack.push_value(bool_to_f64(*lhs $op 0.0))?;
            }
            (AwkValueVariant::UninitializedScalar, AwkValueVariant::Number(rhs)) => {
                $stack.push_value(bool_to_f64(0.0 $op *rhs))?;
            }
            (_, _) => {
                $stack.push_value(bool_to_f64(lhs.scalar_to_string($convfmt)? $op rhs.scalar_to_string($convfmt)?));
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
    ) -> Result<AwkValue, String> {
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
                    let result = ere.matches(&CString::new(string).unwrap());
                    stack.push_value(bool_to_f64(result))?;
                }
                OpCode::Concat => {
                    let rhs = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let mut lhs = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    lhs.push_str(&rhs);
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
                OpCode::AsNumber => {
                    let val = stack.pop_scalar_value()?;
                    stack.push_value(val.scalar_as_f64())?;
                }
                OpCode::GlobalRef(index) => unsafe {
                    // globals outlive the stack, so this is safe
                    stack.push_ref(self.globals[index as usize].get())?
                },
                OpCode::LocalRef(index) => stack.push_ref_to_stack_element(index as usize)?,
                OpCode::FieldRef => {
                    let index = stack.pop_scalar_value()?.scalar_as_f64();
                    if index < 0.0 || index > 1024.0 {
                        return Err("invalid field index".to_string());
                    }
                    let index = index as usize;
                    unsafe { stack.push_ref(record.fields[index].get())? };
                }
                OpCode::Assign => {
                    let value = stack.pop_scalar_value()?;
                    let lvalue = stack.pop_ref();
                    // FIXME: we don't need to convert values here, just check
                    lvalue.ensure_value_is_scalar()?;
                    fields_state = lvalue.assign(value.clone(), global_env)?;
                    stack.push_value(value)?;
                }
                OpCode::IndexArray => {
                    let key = stack
                        .pop_scalar_value()?
                        .scalar_to_string(&global_env.convfmt)?;
                    let array = stack.pop_ref().as_array()?;
                    let element_ref = array.get_or_insert_uninitialized(key) as *mut AwkValue;
                    unsafe { stack.push_ref(element_ref)? };
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
                    BuiltinFunction::Sub | BuiltinFunction::Gsub => {
                        if argc == 2 {
                            unsafe { stack.push_ref(record.fields[0].get())? };
                        }
                        fields_state =
                            builtin_gsub(&mut stack, global_env, function == BuiltinFunction::Sub)?;
                    }
                    other => {
                        call_simple_builtin(other, argc, &mut stack, &record.record, global_env)?
                    }
                },
                OpCode::PushConstant(index) => match self.constants[index as usize].clone() {
                    Constant::Number(num) => stack.push_value(num)?,
                    Constant::String(s) => stack.push_value(s)?,
                    Constant::Regex(ere) => {
                        stack.push_value(AwkValue::from_ere(ere, &record.record))?
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
                OpCode::Return => {
                    let return_value = stack.pop_scalar_value()?;
                    stack.restore_caller();
                    stack.push_value(return_value)?;
                }
                OpCode::Invalid => panic!("invalid opcode"),
                other => todo!("{:?}", other),
            }
            match fields_state {
                FieldsState::Ok => {
                    // no need to recompute anything
                }
                FieldsState::NeedToRecomputeFields => {
                    record.last_field = 0;
                    let record_str = unsafe {
                        (*record.fields[0].get_mut())
                            .to_owned()
                            .scalar_to_string(&global_env.convfmt)?
                    };
                    split_record(&record_str, &global_env.fs, |i, s| {
                        let field_index = i + 1;
                        unsafe {
                            *record.fields[field_index].get_mut() =
                                AwkValue::field_ref(s, field_index as u16)
                        };
                    });
                    record.record =
                        CString::new(record_str).map_err(|_| "invalid string".to_string())?;
                }
                FieldsState::NeedToRecomputeRecord { changed_field } => {
                    record.last_field = record.last_field.max(changed_field);
                    unsafe {
                        *self.globals[SpecialVar::Nf as usize].get() =
                            AwkValue::from(record.last_field as f64);
                    }
                    let mut new_record = String::new();
                    for field in record.fields.iter().skip(1).take(record.last_field - 1) {
                        let field_str = unsafe {
                            (*field.get())
                                .clone()
                                .scalar_to_string(&global_env.convfmt)?
                        };
                        write!(new_record, "{}{}", field_str, &global_env.ofs)
                            .expect("error writing to string");
                    }
                    let last_field_str = unsafe {
                        (*record.fields[record.last_field].get())
                            .clone()
                            .scalar_to_string(&global_env.convfmt)?
                    };
                    write!(new_record, "{}", last_field_str).expect("error writing to string");
                    unsafe {
                        *record.fields[0].get() =
                            AwkValue::from(new_record.clone()).to_ref(AwkRefType::Field(0))
                    };
                    record.record = CString::new(new_record).map_err(|_| "invalid string")?;
                }
            }
            fields_state = FieldsState::Ok;
            stack.ip += ip_increment;
        }
        Ok(stack
            .pop()
            .map(|sv| unsafe { sv.to_owned() })
            .unwrap_or(AwkValue::uninitialized()))
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
        *globals[SpecialVar::Subsep as usize].get_mut() = AwkValue::from("\034".to_string())
            .to_ref(AwkRefType::SpecialGlobalVar(SpecialVar::Subsep));

        Self {
            globals,
            constants,
            convfmt: "%.6g".to_string(),
        }
    }
}

pub fn interpret(program: Program, args: Vec<String>) -> Result<(), String> {
    // println!("{:?}", program);
    let args = iter::once(("0".to_string(), AwkValue::from("awk")))
        .chain(
            args.into_iter()
                .enumerate()
                .map(|(index, s)| ((index + 1).to_string(), s.into())),
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

    interpreter.run(
        &program.begin_instructions,
        &program.functions,
        &mut current_record,
        &mut stack,
        &mut global_env,
    )?;

    let mut current_arg_index = 1;
    let mut nr = 1;
    loop {
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
            .get_or_insert_uninitialized(current_arg_index.to_string())
            .to_owned()
            .scalar_to_string(&interpreter.convfmt)?;

        if arg.is_empty() {
            current_arg_index += 1;
            continue;
        }

        interpreter.globals[SpecialVar::Filename as usize]
            .get_mut()
            .value = AwkValue::from(arg.clone()).value;

        if arg == "-" {
            todo!("read from stdin")
        }

        // TODO: check if the arg is an assignment

        // TODO: should probably figure out something better
        let file_contents =
            std::fs::read_to_string(&arg).map_err(|_| format!("could not read file {}", &arg))?;

        let mut fnr = 1;
        let mut next_record_start = 0;
        while next_record_start < file_contents.len() {
            let (record, bytes_read) =
                first_record(&file_contents[next_record_start..], &global_env.rs);

            next_record_start += bytes_read;

            current_record.reset(record, &global_env.fs)?;
            interpreter.globals[SpecialVar::Nf as usize].get_mut().value =
                AwkValue::from(current_record.last_field as f64).value;

            interpreter.globals[SpecialVar::Fnr as usize]
                .get_mut()
                .value = AwkValue::from(fnr as f64).value;
            interpreter.globals[SpecialVar::Nr as usize].get_mut().value =
                AwkValue::from(nr as f64).value;

            for rule in &program.rules {
                let should_execute = match &rule.pattern {
                    Pattern::All => true,
                    Pattern::Expr(e) => interpreter
                        .run(
                            &e,
                            &program.functions,
                            &mut current_record,
                            &mut stack,
                            &mut global_env,
                        )?
                        .scalar_as_bool(),
                    Pattern::Range { .. } => todo!(),
                };
                if should_execute {
                    interpreter.run(
                        &rule.instructions,
                        &program.functions,
                        &mut current_record,
                        &mut stack,
                        &mut global_env,
                    )?;
                }
            }

            fnr += 1;
            nr += 1;
        }

        current_arg_index += 1;
    }

    interpreter.run(
        &program.end_instructions,
        &program.functions,
        &mut current_record,
        &mut stack,
        &mut global_env,
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::regex_from_str;

    const FIRST_GLOBAL_VAR: u32 = SpecialVar::Count as u32;

    fn interpret_expr(
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
    ) -> AwkValue {
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
        let mut interpreter =
            Interpreter::new(Array::default(), Array::default(), constants, global_count);
        interpreter
            .run(
                &instructions,
                &[],
                &mut Record::default(),
                &mut stack,
                &mut GlobalEnv::default(),
            )
            .expect("error running test")
    }

    fn interpret_expr_with_record(
        instructions: Vec<OpCode>,
        constants: Vec<Constant>,
        global_count: usize,
        record_str: String,
    ) -> (AwkValue, Record) {
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
        let mut interpreter =
            Interpreter::new(Array::default(), Array::default(), constants, global_count);
        let mut record = Record::from_string(record_str, &FieldSeparator::Default)
            .expect("error splitting record");

        let value = interpreter
            .run(
                &instructions,
                &[],
                &mut record,
                &mut stack,
                &mut GlobalEnv::default(),
            )
            .expect("error running test");
        (value, record)
    }

    fn test_global(instructions: Vec<OpCode>, constants: Vec<Constant>) -> AwkValue {
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
        let mut interpreter = Interpreter::new(Array::default(), Array::default(), constants, 1);
        interpreter
            .run(
                &instructions,
                &[],
                &mut Record::default(),
                &mut stack,
                &mut GlobalEnv::default(),
            )
            .expect("error running test");
        interpreter.globals[FIRST_GLOBAL_VAR as usize]
            .get_mut()
            .clone()
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
            )
            .expect("error running test")
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
        if let AwkValueVariant::String(s) = result.value {
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
            AwkValue::from(1.0)
        );

        let constant = vec![Constant::String("hello".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
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
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(2.0)
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
            AwkValue::from(22.0)
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

        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(12.0)
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
            AwkValue::from(0.0)
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
            AwkValue::from(8.0)
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
            AwkValue::from("helloworld".to_string())
        );
    }

    #[test]
    fn test_compare_same_operand_type() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::Number(3.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(1.0)
        );

        let constant = vec![
            Constant::String("abcd".to_string()),
            Constant::String("efgh".to_string()),
        ];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(0.0)
        );
    }

    #[test]
    fn test_compare_number_string() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Le];
        let constant = vec![Constant::Number(2.0), Constant::String("2.".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(1.0)
        );

        let constant = vec![Constant::String("abcd".to_string()), Constant::Number(3.0)];
        let instructions = vec![OpCode::PushConstant(0), OpCode::PushConstant(1), OpCode::Ge];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_compare_number_uninitialized() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::Ge,
        ];
        let constant = vec![Constant::Number(2.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_interpret_in_for_global_array() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            AwkValue::from(0.0)
        );

        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_interpret_in_for_local_array_ref() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::LocalRef(0),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            AwkValue::from(0.0)
        );

        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushOne,
            OpCode::Assign,
            OpCode::Pop,
            OpCode::LocalRef(0),
            OpCode::PushConstant(0),
            OpCode::In,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(
            interpret_expr(instructions, constant, 1),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_negate() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Negate];
        let constant = vec![Constant::Number(456.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(-456.0)
        );
    }

    #[test]
    fn test_not() {
        let instructions = vec![OpCode::PushConstant(0), OpCode::Not];
        let constant = vec![Constant::Number(0.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_postinc() {
        let instructions = vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PostInc];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
    }

    #[test]
    fn test_postdec() {
        let instructions = vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PostDec];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
    }

    #[test]
    fn test_preinc() {
        let instructions = vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PreInc];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(1.0));
    }

    #[test]
    fn test_predec() {
        let instructions = vec![OpCode::GlobalRef(FIRST_GLOBAL_VAR), OpCode::PreDec];
        assert_eq!(test_global(instructions, vec![]), AwkValue::from(-1.0));
    }

    #[test]
    fn test_assign_to_global_var() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
        ];
        let constant = vec![Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), AwkValue::from(123.0));
    }

    #[test]
    fn test_assign_to_array_element() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(
            test_global(instructions, constant),
            Array::from_iter([("key", 123.0)]).into()
        );
    }

    #[test]
    fn test_delete_global_array_element_after_insertion() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_delete_global_array_element_after_insertion_through_local_ref() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushConstant(1),
            OpCode::Assign,
            OpCode::Pop,
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::LocalRef(0),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_delete_from_empty_global_array() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Delete,
        ];
        let constant = vec![Constant::String("key".to_string())];
        assert_eq!(test_global(instructions, constant), Array::default().into());
    }

    #[test]
    fn test_assign_to_local_var() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::LocalRef(0),
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![
            Constant::Number(0.0),
            Constant::String("test string".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from("test string".to_string())
        );
    }

    #[test]
    fn test_assign_to_array_through_local_ref() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::LocalRef(0),
            OpCode::PushConstant(0),
            OpCode::IndexArray,
            OpCode::PushConstant(1),
            OpCode::Assign,
        ];
        let constant = vec![Constant::String("key".to_string()), Constant::Number(123.0)];
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
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(2.0)
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
            AwkValue::from(2.0)
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
            AwkValue::from(3.0)
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
            AwkValue::from("test".to_string())
        );
    }

    #[test]
    fn test_call_with_uninitialized_scalar_argument() {
        let main = vec![OpCode::PushUninitialized, OpCode::Call { id: 0, argc: 1 }];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![OpCode::LocalRef(0), OpCode::Return],
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
                OpCode::LocalRef(0),
                OpCode::PushConstant(0),
                OpCode::IndexArray,
                OpCode::Return,
            ],
        }];
        let constant = vec![Constant::String("key".to_string())];
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
            instructions: vec![OpCode::LocalRef(0), OpCode::PushOne, OpCode::Add],
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
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::Call { id: 0, argc: 1 },
        ];
        let functions = vec![Function {
            parameters_count: 1,
            instructions: vec![
                OpCode::LocalRef(0),
                OpCode::PushConstant(0),
                OpCode::IndexArray,
                OpCode::PushOne,
                OpCode::Assign,
            ],
        }];
        let constants = vec![Constant::String("key".to_string())];
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
                OpCode::LocalRef(0),
                OpCode::LocalRef(1),
                OpCode::LocalRef(2),
                OpCode::LocalRef(3),
                OpCode::LocalRef(4),
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
        let (value, _) = interpret_expr_with_record(instructions, constant, 0, "hello".to_string());
        assert_eq!(value, AwkValue::from("hello".to_string()));
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
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];

        let mut interpreter = Interpreter::new(Array::default(), Array::default(), constants, 0);
        interpreter
            .run(
                &instructions,
                &[],
                &mut Record::from_string("1".to_string(), &FieldSeparator::Default).unwrap(),
                &mut stack,
                &mut Default::default(),
            )
            .unwrap();
        assert_eq!(
            *interpreter.globals[SpecialVar::Nf as usize].get_mut(),
            AwkValue::from(9.0)
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
            AwkValue::from(3.0)
        );

        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::String("z".to_string()),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(0.0)
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
            AwkValue::from(5.0)
        );

        let instructions = vec![OpCode::CallBuiltin {
            function: BuiltinFunction::Length,
            argc: 0,
        }];
        let (value, _) =
            interpret_expr_with_record(instructions, vec![], 0, "test record".to_string());
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
            Constant::String("hello".to_string()),
            Constant::Number(2.0),
            Constant::Number(2.0),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
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
        let constant = vec![Constant::String("hello".to_string()), Constant::Number(3.0)];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from("llo".to_string())
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
    fn test_match_op() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::Match,
        ];
        let constant = vec![
            Constant::String("hello".to_string()),
            Constant::Regex(Rc::new(
                Regex::new(CString::new("e").unwrap()).expect("failed to compile regex"),
            )),
        ];
        assert_eq!(
            interpret_expr(instructions, constant, 0),
            AwkValue::from(1.0)
        );
    }

    #[test]
    fn test_builtin_match() {
        let mut stack = vec![StackValue::Value(AwkValue::uninitialized()); 250];
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Match,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::String("this is a test".to_string()),
            Constant::Regex(Rc::new(regex_from_str("is* a"))),
        ];

        let mut interpreter = Interpreter::new(Array::default(), Array::default(), constants, 0);
        let result = interpreter
            .run(
                &instructions,
                &[],
                &mut Record::default(),
                &mut stack,
                &mut GlobalEnv::default(),
            )
            .expect("error running test");

        assert_eq!(result, AwkValue::from(6.0));
        assert_eq!(
            *interpreter.globals[SpecialVar::Rstart as usize].get_mut(),
            AwkValue::from(6.0)
        );
        assert_eq!(
            *interpreter.globals[SpecialVar::Rlength as usize].get_mut(),
            AwkValue::from(4.0)
        );
    }

    #[test]
    fn test_builtin_split_with_split_ere() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Split,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::String("a, b, c".to_string()),
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
            OpCode::PushConstant(0),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Split,
                argc: 2,
            },
        ];
        let constants = vec![Constant::String("a b c".to_string())];

        let global = test_global(instructions, constants);
        assert_eq!(
            global,
            Array::from_iter([("1", "a"), ("2", "b"), ("3", "c")]).into()
        );
    }

    #[test]
    fn test_builtin_sub() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Sub,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::String("aaabbbabaabb".to_string()),
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::String("x".to_string()),
        ];

        let global = test_global(instructions, constants);
        assert_eq!(global, "aaxabaabb".into());
    }

    #[test]
    fn test_builtin_sub_on_record() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Sub,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::String("x".to_string()),
        ];
        let (_, mut record) =
            interpret_expr_with_record(instructions, constants, 0, "aaabbb ab aabb".to_string());
        assert_eq!(
            *record.fields[0].get_mut(),
            AwkValue::field_ref("aax ab aabb", 0)
        );
        assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
        assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("ab", 2));
        assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("aabb", 3));
    }

    #[test]
    fn test_builtin_gsub() {
        let instructions = vec![
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::PushConstant(0),
            OpCode::Assign,
            OpCode::PushConstant(1),
            OpCode::PushConstant(2),
            OpCode::GlobalRef(FIRST_GLOBAL_VAR),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Gsub,
                argc: 3,
            },
        ];
        let constants = vec![
            Constant::String("aaabbbabaabb".to_string()),
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::String("x".to_string()),
        ];

        let global = test_global(instructions, constants);
        assert_eq!(global, "aaxxax".into());
    }

    #[test]
    fn test_builtin_gsub_on_record() {
        let instructions = vec![
            OpCode::PushConstant(0),
            OpCode::PushConstant(1),
            OpCode::CallBuiltin {
                function: BuiltinFunction::Gsub,
                argc: 2,
            },
        ];
        let constants = vec![
            Constant::Regex(Rc::from(regex_from_str("ab+"))),
            Constant::String("x".to_string()),
        ];
        let (_, mut record) =
            interpret_expr_with_record(instructions, constants, 0, "aaabbb ab aabb".to_string());
        assert_eq!(
            *record.fields[0].get_mut(),
            AwkValue::field_ref("aax x ax", 0)
        );
        assert_eq!(*record.fields[1].get_mut(), AwkValue::field_ref("aax", 1));
        assert_eq!(*record.fields[2].get_mut(), AwkValue::field_ref("x", 2));
        assert_eq!(*record.fields[3].get_mut(), AwkValue::field_ref("ax", 3));
    }
}
