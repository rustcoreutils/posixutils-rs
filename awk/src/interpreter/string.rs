//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;
use std::{ffi::CString, ops::Deref, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
enum AwkStringVariant {
    Owned(String),
    Shared(Rc<str>),
}

#[derive(Debug, Clone)]
pub struct AwkString {
    value: AwkStringVariant,
    is_numeric: bool,
}

impl AwkString {
    pub fn share(&mut self) -> Self {
        match &self.value {
            AwkStringVariant::Owned(ref value) => {
                let value = AwkStringVariant::Shared(Rc::from(value.as_str()));
                Self {
                    value,
                    is_numeric: self.is_numeric,
                }
            }
            AwkStringVariant::Shared(value) => Self {
                value: AwkStringVariant::Shared(value.clone()),
                is_numeric: self.is_numeric,
            },
        }
    }

    pub fn into_shared(self) -> Rc<str> {
        match self.value {
            AwkStringVariant::Owned(value) => Rc::from(value),
            AwkStringVariant::Shared(value) => value,
        }
    }

    pub fn as_str(&self) -> &str {
        match &self.value {
            AwkStringVariant::Owned(value) => value,
            AwkStringVariant::Shared(value) => value,
        }
    }

    pub fn concat(&mut self, other: &AwkString) {
        match &mut self.value {
            AwkStringVariant::Owned(value) => value.push_str(other.as_str()),
            AwkStringVariant::Shared(value) => {
                let mut new_value = value.to_string();
                new_value.push_str(other.as_str());
                self.value = AwkStringVariant::Owned(new_value);
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    pub fn is_numeric(&self) -> bool {
        self.is_numeric
    }

    pub fn numeric_string<V: Into<AwkString>>(val: V) -> AwkString {
        let mut result = val.into();
        result.is_numeric = true;
        result
    }
}

impl Default for AwkString {
    fn default() -> Self {
        AwkString {
            value: AwkStringVariant::Owned(String::new()),
            is_numeric: false,
        }
    }
}

impl AsRef<str> for AwkString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for AwkString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl PartialEq for AwkString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str() && self.is_numeric == other.is_numeric
    }
}

impl fmt::Display for AwkString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<Rc<str>> for AwkString {
    fn from(value: Rc<str>) -> Self {
        AwkString {
            value: AwkStringVariant::Shared(value),
            is_numeric: false,
        }
    }
}

impl From<String> for AwkString {
    fn from(value: String) -> Self {
        AwkString {
            value: AwkStringVariant::Owned(value),
            is_numeric: false,
        }
    }
}

impl From<&str> for AwkString {
    fn from(value: &str) -> Self {
        AwkString {
            value: AwkStringVariant::Shared(value.into()),
            is_numeric: false,
        }
    }
}

impl Into<Rc<str>> for AwkString {
    fn into(self) -> Rc<str> {
        self.into_shared()
    }
}

impl TryInto<CString> for AwkString {
    type Error = String;

    fn try_into(self) -> Result<CString, Self::Error> {
        match self.value {
            AwkStringVariant::Owned(value) => {
                CString::new(value).map_err(|_| "invalid string".to_string())
            }
            AwkStringVariant::Shared(value) => {
                CString::new(value.as_bytes()).map_err(|_| "invalid string".to_string())
            }
        }
    }
}
