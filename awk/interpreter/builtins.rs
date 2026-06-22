//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
    parse_conversion_specifier_args, FormatArgs, IntegerFormat,
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
                let (specifier, mut args) = parse_conversion_specifier_args(&mut iter)?;
                if specifier == '%' {
                    result.push('%');
                    next = iter.next();
                    continue;
                }

                // A '*' field width or precision consumes the next argument(s),
                // in order: width, then precision, then the conversion's value.
                if args.needs_width_arg() {
                    if current_arg == 0 {
                        return Err("not enough arguments for format string".to_string());
                    }
                    current_arg -= 1;
                    args.set_width(
                        swap_with_default(&mut values[current_arg]).scalar_as_f64() as i64
                    );
                }
                if args.needs_precision_arg() {
                    if current_arg == 0 {
                        return Err("not enough arguments for format string".to_string());
                    }
                    current_arg -= 1;
                    args.set_precision(
                        swap_with_default(&mut values[current_arg]).scalar_as_f64() as i64
                    );
                }

                if current_arg == 0 {
                    return Err("not enough arguments for format string".to_string());
                }
                current_arg -= 1;
                let value = swap_with_default(&mut values[current_arg]);
                format_one_conversion(&mut result, specifier, value, &args, float_format)?;
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

/// Format a single `%` conversion (`specifier` with `args`, using the
/// already-fetched argument `value`) and append it to `result`.
fn format_one_conversion(
    result: &mut String,
    specifier: char,
    value: AwkValue,
    args: &FormatArgs,
    float_format: &str,
) -> Result<(), String> {
    match specifier {
        'd' | 'i' => {
            let value = value.scalar_as_f64() as i64;
            fmt_write_signed(result, value, args);
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
            fmt_write_unsigned(result, value as u64, format, args);
        }
        'a' | 'A' => {
            let value = value.scalar_as_f64();
            fmt_write_hex_float(result, value, specifier == 'a', args);
        }
        'f' | 'F' => {
            let value = value.scalar_as_f64();
            fmt_write_decimal_float(result, value, specifier == 'f', args);
        }
        'e' | 'E' => {
            let value = value.scalar_as_f64();
            fmt_write_scientific_float(result, value, specifier == 'e', args);
        }
        'g' | 'G' => {
            let value = value.scalar_as_f64();
            fmt_write_float_general(result, value, specifier == 'g', args);
        }
        'c' => {
            let ch = match &value.value {
                AwkValueVariant::Number(n) => char::from_u32(*n as u32).unwrap_or('\0'),
                AwkValueVariant::String(s) if s.is_numeric => {
                    let code = value.scalar_as_f64() as u32;
                    char::from_u32(code).unwrap_or('\0')
                }
                AwkValueVariant::String(s) if !s.is_empty() => s.chars().next().unwrap(),
                _ => {
                    let code = value.scalar_as_f64() as u32;
                    char::from_u32(code).unwrap_or('\0')
                }
            };
            let ch_str = ch.to_string();
            fmt_write_string(result, &ch_str, args);
        }
        's' => {
            let value = value.scalar_to_string(float_format)?;
            fmt_write_string(result, &value, args);
        }
        _ => return Err(format!("unsupported format specifier '{}'", specifier)),
    }
    Ok(())
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

/// Convert a byte offset within `s` to a character offset (the number of whole
/// characters that begin before `byte`). POSIX awk string functions operate on
/// characters, while the regex engine and `str` searches report byte offsets.
pub(crate) fn byte_offset_to_char_count(s: &str, byte: usize) -> usize {
    s.char_indices().take_while(|&(i, _)| i < byte).count()
}

/// `substr(s, m[, n])`: the at most `n`-character substring of `s` that begins
/// at character position `m` (numbering from 1). Positions below 1 still consume
/// part of `n`, matching nawk/gawk, so e.g. `substr("hello", -1, 3) == "h"`.
/// `m` and `n` are assumed already truncated toward zero by the caller. The
/// character window is `[max(m, 1), m + n)`; `take` naturally stops at the end
/// of the string, so no length pre-scan or `Vec` is needed.
pub(crate) fn substr(s: &str, m: i64, n: Option<i64>) -> String {
    let start = m.max(1);
    let count = match n {
        None => usize::MAX,
        Some(n) => m.saturating_add(n).saturating_sub(start).max(0) as usize,
    };
    // `start >= 1`; a start past `usize::MAX` (only possible on a 32-bit `usize`)
    // skips the whole string, yielding the empty substring.
    let skip = usize::try_from(start - 1).unwrap_or(usize::MAX);
    s.chars().skip(skip).take(count).collect()
}

pub(crate) fn builtin_match(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
) -> Result<(f64, f64), String> {
    let ere = stack.pop_value().into_ere()?;
    let string = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    let text = string.as_str().to_owned();
    let mut locations = ere.match_locations(string.try_into()?);
    let start;
    let len;
    if let Some(first_match) = locations.next() {
        // RSTART/RLENGTH are measured in characters, not bytes.
        let cstart = byte_offset_to_char_count(&text, first_match.start);
        let cend = byte_offset_to_char_count(&text, first_match.end);
        start = cstart as i64 + 1;
        len = (cend - cstart) as i64;
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
                Some('&') => current_repl_part.push('&'),
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
            result.push_str(replaced_string);
            result.push_str(part);
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

/// `split(s, arr[, fs])`: split `s` into `arr` on the field separator (the
/// optional third argument, else `FS`) and return the number of fields. When the
/// `fs` argument is a regex value it is used directly; otherwise it is
/// interpreted like `FS` (e.g. `" "` means whitespace).
pub(crate) fn builtin_split(
    stack: &mut Stack,
    global_env: &mut GlobalEnv,
    argc: u16,
) -> Result<FieldsState, String> {
    let separator = if argc == 2 {
        None
    } else {
        let sep_val = stack.pop_value();
        if matches!(&sep_val.value, AwkValueVariant::Regex { .. }) {
            Some(FieldSeparator::Ere(sep_val.into_ere()?))
        } else {
            let sep_str = sep_val.scalar_to_string(&global_env.convfmt)?;
            Some(FieldSeparator::try_from(sep_str)?)
        }
    };
    let s = stack
        .pop_scalar_value()?
        .scalar_to_string(&global_env.convfmt)?;
    let array = stack.pop_ref().as_array()?;
    array.clear();

    if !s.is_empty() {
        split_record(
            s,
            separator.iter().next().unwrap_or(&global_env.fs),
            |i, s| array.set((i + 1).to_string(), s).map(|_| ()),
        )?;
    }
    let n = array.len();
    stack.push_value(n as f64)?;
    Ok(FieldsState::Ok)
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
            // index() returns a character position, numbering from 1; str::find
            // reports a byte offset, so convert it to a character count.
            let index = s
                .as_str()
                .find(t.as_str())
                .map(|i| byte_offset_to_char_count(s.as_str(), i) as f64 + 1.0)
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
                    // length() counts characters, not bytes.
                    let value_str = value.scalar_to_string(&global_env.convfmt)?;
                    stack.push_value(value_str.chars().count() as f64)?;
                }
            }
        }
        BuiltinFunction::Split => return builtin_split(stack, global_env, argc),
        BuiltinFunction::Sprintf => {
            let str = builtin_sprintf(stack, argc, global_env)?;
            stack.push_value(str)?;
        }
        BuiltinFunction::Substr => {
            let n = if argc == 2 {
                None
            } else {
                Some(stack.pop_scalar_value()?.scalar_as_f64().trunc() as i64)
            };
            let m = stack.pop_scalar_value()?.scalar_as_f64().trunc() as i64;
            let s = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            stack.push_value(substr(s.as_str(), m, n))?;
        }
        BuiltinFunction::ToLower => {
            // POSIX: case mapping follows the LC_CTYPE category of the locale.
            let value = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let lowered: String = value.chars().map(plib::locale::to_lower).collect();
            stack.push_value(lowered)?;
        }
        BuiltinFunction::ToUpper => {
            let value = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?;
            let uppered: String = value.chars().map(plib::locale::to_upper).collect();
            stack.push_value(uppered)?;
        }
        BuiltinFunction::Gsub | BuiltinFunction::Sub => {
            return builtin_gsub(stack, global_env, function == BuiltinFunction::Sub)
        }
        BuiltinFunction::System => {
            let command: CString = stack
                .pop_scalar_value()?
                .scalar_to_string(&global_env.convfmt)?
                .try_into()?;
            stack.push_value(run_system(&command) as f64)?;
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

/// Run `command` via `libc::system` and translate its wait-status into the value
/// awk's `system()` returns: the exit code for a normal exit, `128 + signal` for
/// a signalled child, or -1 when the command could not be run.
fn run_system(command: &CString) -> i32 {
    let status = unsafe { libc::system(command.as_ptr()) };
    if status == -1 {
        -1
    } else if libc::WIFEXITED(status) {
        libc::WEXITSTATUS(status)
    } else if libc::WIFSIGNALED(status) {
        128 + libc::WTERMSIG(status)
    } else {
        -1
    }
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
