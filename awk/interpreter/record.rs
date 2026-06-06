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
use super::value::{AwkRefType, AwkValue, AwkValueRef};
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

/// Build the stored value for field `index` from its text. POSIX 85511: a field
/// created from `$0`/FS that "does not contain any characters" has the
/// uninitialized value (so e.g. an empty field compares numerically equal to 0),
/// while a non-empty field is a (possibly numeric) string.
fn make_field(value: AwkString, index: u16) -> AwkValue {
    if value.is_empty() {
        AwkValue::uninitialized_scalar().into_ref(AwkRefType::Field(index))
    } else {
        AwkValue::field_ref(value, index)
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
    /// Field storage, grown on demand. Each field lives in its own `Box`, so a
    /// raw pointer obtained from a cell stays valid even when the outer `Vec` is
    /// reallocated by a later growth — the operand stack holds such pointers
    /// across opcodes. Index 0 is `$0`; index `i` is `$i`. (The boxing is load
    /// bearing for pointer stability, so `clippy::vec_box` does not apply.)
    #[allow(clippy::vec_box)]
    fields: RefCell<Vec<Box<AwkValueRef>>>,
    pub(crate) last_field: RefCell<usize>,
}

impl Record {
    /// Hard ceiling on the field index, imposed by `AwkRefType::Field(u16)`.
    pub(crate) const MAX_FIELDS: usize = u16::MAX as usize;

    fn new_field_cell(index: usize) -> Box<AwkValueRef> {
        Box::new(AwkValueRef::new(
            AwkValue::uninitialized_scalar().into_ref(AwkRefType::Field(index as u16)),
        ))
    }

    /// Grow `fields` (in place, reusing existing boxes) so that exactly
    /// `field_index` numbered fields plus `$0` are present, assigning each from
    /// the split values; surplus fields from a previous longer record are
    /// dropped.
    #[allow(clippy::vec_box)] // boxed for pointer stability; see `fields`
    fn fill_fields(
        fields: &mut Vec<Box<AwkValueRef>>,
        record: AwkString,
        field_separator: &FieldSeparator,
        previous_last_field: usize,
    ) -> Result<usize, String> {
        let mut last_field = 0;
        split_record(record, field_separator, |i, s| {
            let field_index = i + 1;
            if field_index > Record::MAX_FIELDS {
                return Err("too many fields".to_string());
            }
            while fields.len() <= field_index {
                let next = fields.len();
                fields.push(Record::new_field_cell(next));
            }
            *fields[field_index].get_mut() = make_field(s, field_index as u16);
            last_field = field_index;
            Ok(())
        })?;
        Self::clear_fields_above(fields, last_field, previous_last_field);
        Ok(last_field)
    }

    /// Reset fields `new_last + 1 ..= previous_last` to the uninitialized value
    /// (the fields a now-shorter record no longer has). The boxes are kept, not
    /// dropped — a pointer to one may still be live on the operand stack (e.g.
    /// `$9 = ($0 = "a b")`), so dropping it would dangle that pointer; storage
    /// therefore grows to a high-water mark but never shrinks. Only fields up to
    /// `previous_last` are touched, since everything above it is already
    /// uninitialized.
    fn clear_fields_above(fields: &mut [Box<AwkValueRef>], new_last: usize, previous_last: usize) {
        let upper = previous_last.min(fields.len().saturating_sub(1));
        for (idx, cell) in fields
            .iter_mut()
            .enumerate()
            .take(upper + 1)
            .skip(new_last + 1)
        {
            *cell.get_mut() =
                AwkValue::uninitialized_scalar().into_ref(AwkRefType::Field(idx as u16));
        }
    }

    /// Pointer to field `index`, growing the backing storage with uninitialized
    /// fields if necessary so the slot exists (used for lvalues like `$n = …`).
    /// The returned pointer is stable across later growth (boxed cell).
    pub(crate) fn field_ref_ptr(&self, index: usize) -> *mut AwkValue {
        let mut fields = self.fields.borrow_mut();
        while fields.len() <= index {
            let next = fields.len();
            fields.push(Self::new_field_cell(next));
        }
        fields[index].get()
    }

    /// A clone of field `index` for reading. A reference to a nonexistent field
    /// (beyond the current record) yields the uninitialized value and does NOT
    /// create the field, per POSIX.
    pub(crate) fn read_field(&self, index: usize) -> AwkValue {
        let fields = self.fields.borrow();
        match fields.get(index) {
            // safe: fields are never arrays
            Some(cell) => unsafe { (*cell.get()).clone() },
            None => AwkValue::uninitialized_scalar().into_ref(AwkRefType::Field(index as u16)),
        }
    }

    pub(crate) fn reset(
        &mut self,
        record: String,
        field_separator: &FieldSeparator,
    ) -> Result<(), String> {
        let record = maybe_numeric_string(record);
        let previous_last_field = *self.last_field.get_mut();
        let fields = self.fields.get_mut();
        let last_field =
            Self::fill_fields(fields, record.clone(), field_separator, previous_last_field)?;
        *fields[0].get_mut() = AwkValue::field_ref(record.clone(), 0);
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
        if last_field > Self::MAX_FIELDS {
            return Err("too many fields".to_string());
        }
        let previous_last_field = *self.last_field.borrow();
        let last_field = if truncate_fields {
            // NF was assigned: the record now has exactly `last_field` fields.
            last_field
        } else {
            previous_last_field.max(last_field)
        };
        // Ensure the backing storage spans the (possibly grown) field range, and
        // clear any surplus fields when NF shrank the record.
        {
            let mut fields = self.fields.borrow_mut();
            while fields.len() <= last_field {
                let next = fields.len();
                fields.push(Self::new_field_cell(next));
            }
            if truncate_fields {
                Self::clear_fields_above(fields.as_mut_slice(), last_field, previous_last_field);
            }
        }
        if last_field == 0 {
            let record_str = maybe_numeric_string(String::new());
            *self.fields.borrow()[0].get() = AwkValue::field_ref(record_str.clone(), 0);
            *self.record.borrow_mut() = record_str.try_into()?;
            *self.last_field.borrow_mut() = 0;
            return Ok(());
        }
        let mut new_record = String::new();
        {
            let fields = self.fields.borrow();
            for cell in fields.iter().skip(1).take(last_field - 1) {
                let field_str = (*cell.get())
                    .clone()
                    .scalar_to_string(&global_env.convfmt)?;
                write!(new_record, "{}{}", field_str, &global_env.ofs)
                    .expect("error writing to string");
            }
            let last_field_str = (*fields[last_field].get())
                .clone()
                .scalar_to_string(&global_env.convfmt)?;
            write!(new_record, "{}", last_field_str).expect("error writing to string");
        }
        // the spec doesn't specify if a recomputed record should be a numeric string.
        // Most other implementations don't really handle this case. Here we just
        // mark it as a numeric string if appropriate
        let record_str = maybe_numeric_string(new_record);
        *self.fields.borrow()[0].get() = AwkValue::field_ref(record_str.clone(), 0);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    /// # Safety
    /// The caller has to ensure that there are no active references to the fields
    pub(crate) unsafe fn recompute_fields(&self, global_env: &GlobalEnv) -> Result<(), String> {
        let record_str = self.read_field(0).scalar_to_string(&global_env.convfmt)?;
        let previous_last_field = *self.last_field.borrow();
        let mut fields = self.fields.borrow_mut();
        let last_field = Self::fill_fields(
            &mut fields,
            record_str.clone(),
            &global_env.fs,
            previous_last_field,
        )?;
        *fields[0].get_mut() = AwkValue::field_ref(record_str.clone(), 0);
        drop(fields);
        *self.record.borrow_mut() = record_str.try_into()?;
        *self.last_field.borrow_mut() = last_field;
        Ok(())
    }

    pub(crate) fn get_last_field(&self) -> usize {
        *self.last_field.borrow()
    }

    /// Test-only accessor returning a clone of field `index`.
    #[cfg(test)]
    pub(crate) fn field(&self, index: usize) -> AwkValue {
        self.read_field(index)
    }
}

impl Default for Record {
    fn default() -> Self {
        // Only `$0` is allocated up front; numbered fields grow on demand.
        let fields = vec![Record::new_field_cell(0)];
        Self {
            record: CString::default().into(),
            fields: RefCell::new(fields),
            last_field: 0.into(),
        }
    }
}

pub(crate) fn is_valid_record_index(index: usize) -> Result<(), String> {
    if index > Record::MAX_FIELDS {
        Err("invalid field index".to_string())
    } else {
        Ok(())
    }
}
