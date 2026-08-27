//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::shell::Display;
use crate::shstr::{ShStr, ShString};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Clone, Default)]
pub struct Value {
    /// `None` if `Value` is unset
    pub value: Option<ShString>,
    pub export: bool,
    pub readonly: bool,
}

impl Value {
    pub fn new_exported<V: Into<ShString>>(value: V) -> Self {
        Value {
            value: Some(value.into()),
            export: true,
            readonly: false,
        }
    }

    pub fn new<V: Into<ShString>>(value: V) -> Self {
        Value {
            value: Some(value.into()),
            export: false,
            readonly: false,
        }
    }

    pub fn export_or(&mut self, value: bool) {
        self.export = self.export || value;
    }
}

pub type GlobalScope = HashMap<String, Value>;
pub type LocalScope = HashMap<String, ShString>;

#[derive(Default, Clone)]
pub struct Environment {
    global_scope: HashMap<String, Value>,
    /// variables in the local scope are implicitly export.
    /// For example, if `f` is a function and we execute:
    /// ```sh
    /// var=value f
    /// ```
    /// `var` will be available to all commands called from `f`.
    /// (This is also true in other shells)
    local_scopes: Vec<LocalScope>,
}

#[derive(Debug, Clone)]
pub struct CannotModifyReadonly(String);

impl Display for CannotModifyReadonly {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "sh: cannot set readonly variable {}", self.0)
    }
}

impl Environment {
    pub fn set_global<V: Into<ShString>>(
        &mut self,
        name: String,
        value: V,
    ) -> Result<&mut Value, CannotModifyReadonly> {
        let value = value.into();
        self.remove_from_local_scope(&name);
        match self.global_scope.entry(name) {
            Entry::Occupied(mut e) => {
                if e.get().readonly {
                    return Err(CannotModifyReadonly(e.key().clone()));
                }
                e.get_mut().value = Some(value);
                Ok(e.into_mut())
            }
            Entry::Vacant(e) => Ok(e.insert(Value {
                value: Some(value),
                export: false,
                readonly: false,
            })),
        }
    }

    pub fn set_global_forced<V: Into<ShString>>(&mut self, name: String, value: V) -> &mut Value {
        let value = value.into();
        self.remove_from_local_scope(&name);
        match self.global_scope.entry(name) {
            Entry::Occupied(mut e) => {
                e.get_mut().value = Some(value);
                e.into_mut()
            }
            Entry::Vacant(e) => e.insert(Value {
                value: Some(value),
                export: false,
                readonly: false,
            }),
        }
    }

    pub fn set_global_if_unset(&mut self, name: &str, value: &str) {
        if !self.global_scope.contains_key(name) {
            self.global_scope
                .insert(name.to_string(), Value::new(ShString::from(value)));
        }
    }

    pub fn set<V: Into<ShString>>(
        &mut self,
        name: String,
        value: V,
    ) -> Result<(), CannotModifyReadonly> {
        let value = value.into();
        if let Some(innermost_scope) = self.local_scopes.last_mut() {
            innermost_scope.insert(name, value);
        } else {
            self.set_global(name, value)?;
        }
        Ok(())
    }

    /// The value as text, for the variables whose meaning *is* text — a number
    /// to parse, a locale name, an editor setting. Yields `None` when the value
    /// is not valid UTF-8, which for those variables is not a usable value
    /// anyway. Anything that must survive arbitrary bytes uses
    /// [`Self::get_value`].
    pub fn get_str_value(&self, name: &str) -> Option<&str> {
        self.get_value(name).and_then(|v| v.to_str())
    }

    /// The value as bytes. A shell variable may hold anything.
    pub fn get_value(&self, name: &str) -> Option<&ShStr> {
        for local_scope in self.local_scopes.iter().rev() {
            if let Some(value) = local_scope.get(name).map(|val| val.as_sh_str()) {
                return Some(value);
            }
        }
        self.global_scope
            .get(name)
            .and_then(|val| val.value.as_ref().map(|v| v.as_sh_str()))
    }

    pub fn promote_local_or_get_global(&mut self, name: String) -> &mut Value {
        for local_scope in self.local_scopes.iter_mut().rev() {
            if let Some((k, v)) = local_scope.remove_entry(&name) {
                return self.set_global(k, v).unwrap();
            }
        }
        match self.global_scope.entry(name) {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(Value::default()),
        }
    }

    pub fn unset(&mut self, name: &str) -> Result<(), CannotModifyReadonly> {
        self.remove_from_local_scope(name);
        if let Some(var) = self.global_scope.get_mut(name) {
            if var.readonly {
                return Err(CannotModifyReadonly(name.to_string()));
            }
            var.value = None;
        }
        Ok(())
    }

    fn remove_from_local_scope(&mut self, var: &str) {
        for local_scope in &mut self.local_scopes {
            local_scope.remove(var);
        }
    }

    pub fn push_scope(&mut self) {
        self.local_scopes.push(LocalScope::new());
    }

    pub fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn global_scope(&self) -> &GlobalScope {
        &self.global_scope
    }

    pub fn exported(&self) -> impl Iterator<Item = (&String, &ShStr)> {
        let mut exported = HashMap::with_capacity(self.global_scope.len());
        for (name, var) in &self.global_scope {
            if var.export {
                if let Some(value) = &var.value {
                    exported.insert(name, value.as_sh_str());
                }
            }
        }
        for local_scope in &self.local_scopes {
            for (name, value) in local_scope {
                exported.insert(name, value.as_sh_str());
            }
        }
        exported.into_iter()
    }
}

impl<I: IntoIterator<Item = (String, Value)>> From<I> for Environment {
    fn from(value: I) -> Self {
        Self {
            global_scope: value.into_iter().collect(),
            local_scopes: Vec::default(),
        }
    }
}
