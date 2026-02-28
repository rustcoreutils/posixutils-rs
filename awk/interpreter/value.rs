//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cell::UnsafeCell;
use std::ffi::CString;
use std::rc::Rc;

use super::array::Array;
use super::string::AwkString;
use super::{bool_to_f64, is_integer, sprintf, strtod};
use crate::program::SpecialVar;
use crate::regex::Regex;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(crate) enum AwkValueVariant {
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

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum AwkRefType {
    None,
    Field(u16),
    SpecialGlobalVar(SpecialVar),
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(crate) struct AwkValue {
    pub(crate) value: AwkValueVariant,
    pub(crate) ref_type: AwkRefType,
}

pub(crate) type AwkValueRef = UnsafeCell<AwkValue>;

impl AwkValue {
    pub(crate) fn scalar_as_f64(&self) -> f64 {
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

    pub(crate) fn scalar_as_bool(&self) -> bool {
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

    pub(crate) fn scalar_to_string(self, num_fmt: &str) -> Result<AwkString, String> {
        match self.value {
            AwkValueVariant::Number(num) => {
                if is_integer(num) {
                    Ok((num as i64).to_string().into())
                } else {
                    sprintf(num_fmt, &mut [num.into()], num_fmt)
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

    pub(crate) fn ensure_value_is_scalar(&mut self) -> Result<(), String> {
        match &self.value {
            AwkValueVariant::Uninitialized => self.value = AwkValueVariant::UninitializedScalar,
            AwkValueVariant::Array(_) => return Err("array used in scalar context".into()),
            _ => {
                //already a scalar
            }
        }
        Ok(())
    }

    pub(crate) fn as_array(&mut self) -> Result<&mut Array, String> {
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

    pub(crate) fn assign<V: Into<Self>>(
        &mut self,
        rhs: V,
        global_env: &mut super::GlobalEnv,
    ) -> Result<super::FieldsState, String> {
        let rhs = rhs.into();
        self.value = rhs.value;
        match self.ref_type {
            AwkRefType::SpecialGlobalVar(special_var) => {
                global_env.set(special_var, self)?;
                if special_var == SpecialVar::Nf {
                    return Ok(super::FieldsState::NfChanged);
                }
            }
            AwkRefType::Field(index) => {
                return if index == 0 {
                    Ok(super::FieldsState::RecordChanged)
                } else {
                    Ok(super::FieldsState::FieldChanged {
                        changed_field: index as usize,
                    })
                };
            }
            AwkRefType::None => {}
        }
        Ok(super::FieldsState::Ok)
    }

    pub(crate) fn into_ref(self, ref_type: AwkRefType) -> Self {
        Self { ref_type, ..self }
    }

    pub(crate) fn into_ere(self) -> Result<Rc<Regex>, String> {
        match self.value {
            AwkValueVariant::Regex { ere, .. } => Ok(ere),
            AwkValueVariant::String(s) => Ok(Rc::new(Regex::new(s.try_into()?)?)),
            AwkValueVariant::UninitializedScalar => {
                Ok(Rc::new(Regex::new(CString::new("").unwrap())?))
            }
            _ => Err("expected extended regular expression".to_string()),
        }
    }

    pub(crate) fn uninitialized() -> Self {
        Self {
            value: AwkValueVariant::Uninitialized,
            ref_type: AwkRefType::None,
        }
    }

    pub(crate) fn uninitialized_scalar() -> Self {
        Self {
            value: AwkValueVariant::UninitializedScalar,
            ref_type: AwkRefType::None,
        }
    }

    pub(crate) fn from_ere(ere: Rc<Regex>, record: &CString) -> Self {
        let matches_record = ere.matches(record);
        Self {
            value: AwkValueVariant::Regex {
                ere,
                matches_record,
            },
            ref_type: AwkRefType::None,
        }
    }

    pub(crate) fn field_ref<V: Into<AwkValue>>(value: V, field_index: u16) -> Self {
        let value = value.into();
        value.into_ref(AwkRefType::Field(field_index))
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
