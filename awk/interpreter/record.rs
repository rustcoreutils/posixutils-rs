//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cell::RefCell;
use std::ffi::CString;
use std::fmt::Write;
use std::rc::Rc;

use super::string::AwkString;
use super::value::{AwkRefType, AwkValue, AwkValueRef, AwkValueVariant};
use super::{maybe_numeric_string, GlobalEnv};
use crate::regex::Regex;

pub(crate) enum FieldSeparator {
    Default,
    Char(u8),
    Ere(Rc<Regex>),
    Null,
}

/// Escape a character for use in an ERE pattern.
pub(crate) fn ere_escape_char(c: char) -> String {
    if "\\^$.|?*+()[]{}".contains(c) {
        format!("\\{}", c)
    } else {
        c.to_string()
    }
}

/// Splits a record into fields and calls the provided closure for each field.
/// If the record is a numeric string, fields will be numeric strings if appropriate.
pub(crate) fn split_record<S: FnMut(usize, AwkString) -> Result<(), String>>(
    record: AwkString,
    field_separator: &FieldSeparator,
    mut store_result: S,
) -> Result<(), String> {
    let string = |s: &str| -> AwkString { maybe_numeric_string(s) };
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
            for separator_range in re.match_locations(record.clone().try_into()?) {
                store_result(index, string(&record[split_start..separator_range.start]))?;
                split_start = separator_range.end;
                index += 1;
            }
            store_result(index, string(&record[split_start..]))
        }
        FieldSeparator::Null => record.chars().enumerate().try_for_each(|(i, c)| {
            let mut s = String::new();
            s.push(c);
            store_result(i, string(&s))
        }),
    }
}

impl TryFrom<AwkString> for FieldSeparator {
    type Error = String;

    fn try_from(value: AwkString) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Ok(FieldSeparator::Null)
        } else if value.as_str() == " " {
            Ok(FieldSeparator::Default)
        } else if value.len() == 1 {
            Ok(FieldSeparator::Char(*value.as_bytes().first().unwrap()))
        } else {
            let ere = Regex::new(value.try_into()?)?;
            Ok(FieldSeparator::Ere(Rc::from(ere)))
        }
    }
}

pub(crate) enum FieldsState {
    RecordChanged,
    FieldChanged { changed_field: usize },
    NfChanged,
    Ok,
}

pub(crate) struct Record {
    pub(crate) record: RefCell<CString>,
    pub(crate) fields: Vec<AwkValueRef>,
    pub(crate) last_field: RefCell<usize>,
}

impl Record {
    pub(crate) const MAX_FIELDS: usize = 1024;

    pub(crate) fn reset(
        &mut self,
        record: String,
        field_separator: &FieldSeparator,
    ) -> Result<(), String> {
        let previous_last_field = *self.last_field.get_mut();
        let mut last_field = 0;
        let record = maybe_numeric_string(record);
        split_record(record.clone(), field_separator, |i, s| {
            let field_index = i + 1;
            last_field += 1;
            *self.fields[field_index].get_mut() = AwkValue::field_ref(s, field_index as u16);
            Ok(())
        })
        .expect("error splitting record");
        if last_field < previous_last_field {
            for field in &mut self.fields[last_field + 1..=previous_last_field] {
                field.get_mut().value = AwkValueVariant::UninitializedScalar;
            }
        }
        *self.fields[0].get_mut() = AwkValue::field_ref(record.clone(), 0);
        *self.record.get_mut() = record.try_into()?;
        *self.last_field.get_mut() = last_field;
        Ok(())
    }

    /// # Safety
    /// The caller has to ensure that there are no active references to the fields
    pub(crate) unsafe fn recompute_record(
        &self,
        global_env: &GlobalEnv,
        last_field: usize,
        truncate_fields: bool,
    ) -> Result<(), String> {
        let last_field = if truncate_fields {
            if last_field < *self.last_field.borrow() {
                for field in self
                    .fields
                    .iter()
                    .skip(last_field + 1)
                    .take(*self.last_field.borrow() - last_field)
                {
                    (*field.get()).value = AwkValueVariant::UninitializedScalar;
                }
            }
            last_field
        } else {
            self.last_field.borrow().max(last_field)
        };
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
        // the spec doesn't specify if a recomputed record should be a numeric string.
        // Most other implementations don't really handle this case. Here we just
        // mark it as a numeric string if appropriate
        let record_str = maybe_numeric_string(new_record);
        *self.fields[0].get() = AwkValue::field_ref(record_str.clone(), 0);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    /// # Safety
    /// The caller has to ensure that there are no active references to the fields
    pub(crate) unsafe fn recompute_fields(&self, global_env: &GlobalEnv) -> Result<(), String> {
        let mut last_field = 0;
        let record_str = (*self.fields[0].get())
            .to_owned()
            .scalar_to_string(&global_env.convfmt)?;
        split_record(record_str.clone(), &global_env.fs, |i, s| {
            let field_index = i + 1;
            last_field += 1;
            *self.fields[field_index].get() = AwkValue::field_ref(s, field_index as u16);
            Ok(())
        })
        .expect("error splitting record");
        *self.fields[0].get() = AwkValue::field_ref(record_str.clone(), 0);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    pub(crate) fn get_last_field(&self) -> usize {
        *self.last_field.borrow()
    }
}

impl Default for Record {
    fn default() -> Self {
        let fields = (0..=Record::MAX_FIELDS)
            .map(|i| {
                AwkValueRef::new(
                    AwkValue::uninitialized_scalar().into_ref(AwkRefType::Field(i as u16)),
                )
            })
            .collect();
        Self {
            record: CString::default().into(),
            fields,
            last_field: 0.into(),
        }
    }
}

pub(crate) fn is_valid_record_index(index: usize) -> Result<(), String> {
    if !(0..=Record::MAX_FIELDS).contains(&index) {
        Err("invalid field index".to_string())
    } else {
        Ok(())
    }
}
