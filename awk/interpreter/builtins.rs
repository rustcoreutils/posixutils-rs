//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CString;
use std::fmt::Write;

use super::format::{
    fmt_write_decimal_float, fmt_write_float_general, fmt_write_hex_float,
    fmt_write_scientific_float, fmt_write_signed, fmt_write_string, fmt_write_unsigned,
    parse_conversion_specifier_args, IntegerFormat,
};
use super::record::{split_record, FieldSeparator, FieldsState};
use super::stack::Stack;
use super::string::AwkString;
use super::value::{AwkValue, AwkValueVariant};
use super::{swap_with_default, GlobalEnv};
use crate::program::BuiltinFunction;
use crate::regex::Regex;

pub(crate) fn sprintf(
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
                    'c' => match &value.value {
                        AwkValueVariant::String(s) if !s.is_empty() => {
                            result.push(s.chars().next().unwrap());
                        }
                        _ => {
                            let code = value.scalar_as_f64() as u32;
                            if let Some(c) = char::from_u32(code) {
                                result.push(c);
                            }
                        }
                    },
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
    Ok(result.into())
}

pub(crate) fn builtin_sprintf(
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

pub(crate) fn builtin_match(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
) -> Result<(f64, f64), String> {
    let ere = stack.pop_value().into_ere()?;
    let string = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
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

pub(crate) fn gsub(
    ere: &Regex,
    repl: &str,
    in_str: &str,
    only_replace_first: bool,
) -> Result<(AwkString, usize), String> {
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
    for m in ere.match_locations(AwkString::from(in_str).try_into()?) {
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
    Ok((result.into(), num_replacements))
}

pub(crate) fn builtin_gsub(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
    is_sub: bool,
) -> Result<FieldsState, String> {
    let repl = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    let ere = stack.pop_value().into_ere()?;
    let in_str = stack.pop_ref();
    in_str.ensure_value_is_scalar()?;
    let (result, count) = gsub(
        &ere,
        &repl,
        &in_str.clone().scalar_to_string(&global_env.convfmt)?,
        is_sub,
    )?;
    let result = in_str.assign(result, global_env);
    stack.push_value(count as f64)?;
    result
}

pub(crate) fn call_simple_builtin(
    function: BuiltinFunction,
    argc: u16,
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
) -> Result<FieldsState, String> {
    match function {
        BuiltinFunction::Atan2 => {
            let x = stack.pop_scalar_value()?.scalar_as_f64();
            let y = stack.pop_scalar_value()?.scalar_as_f64();
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
            let value = stack.pop_value();
            match &value.value {
                AwkValueVariant::Array(array) => {
                    stack.push_value(array.len() as f64)?;
                }
                _ => {
                    let value_str = value.scalar_to_string(&global_env.convfmt)?;
                    stack.push_value(value_str.len() as f64)?;
                }
            }
        }
        BuiltinFunction::Split => {
            let separator = if argc == 2 {
                None
            } else {
                Some(FieldSeparator::Ere(stack.pop_value().into_ere()?))
            };
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let array = stack.pop_ref().as_array()?;
            array.clear();

            split_record(
                s,
                separator.iter().next().unwrap_or(&global_env.fs),
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

pub(crate) fn gather_values(stack: &mut Stack, count: u16) -> Result<Vec<AwkValue>, String> {
    let mut values = Vec::with_capacity(count as usize);
    for _ in 0..count {
        values.push(stack.pop_scalar_value()?);
    }
    Ok(values)
}

pub(crate) fn print_to_string(
    stack: &mut Stack,
    argc: u16,
    global_env: &GlobalEnv,
) -> Result<AwkString, String> {
    let mut values = Vec::with_capacity(argc as usize);
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
