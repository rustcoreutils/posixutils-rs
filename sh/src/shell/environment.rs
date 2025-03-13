use crate::shell::Display;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Clone)]
pub struct Value {
    /// `None` if `Value` is unset
    pub value: Option<String>,
    pub export: bool,
    pub readonly: bool,
}

impl Value {
    pub fn new_exported(value: String) -> Self {
        Value {
            value: Some(value),
            export: true,
            readonly: false,
        }
    }

    pub fn new(value: String) -> Self {
        Value {
            value: Some(value),
            export: false,
            readonly: false,
        }
    }

    pub fn is_null(&self) -> bool {
        self.value.as_ref().is_some_and(|v| v.is_empty())
    }

    pub fn is_set(&self) -> bool {
        self.value.is_some()
    }
}

#[derive(Default, Clone)]
pub struct Environment {
    pub variables: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct CannotModifyReadonly(String);

impl CannotModifyReadonly {
    pub fn var_name(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for CannotModifyReadonly {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "sh: cannot set readonly variable {}", self.0)
    }
}

impl Environment {
    pub fn set(
        &mut self,
        name: String,
        value: Option<String>,
        add_export: bool,
        add_readonly: bool,
    ) -> Result<(), CannotModifyReadonly> {
        match self.variables.entry(name) {
            Entry::Occupied(mut e) => {
                if let Some(value) = value {
                    if e.get().readonly {
                        return Err(CannotModifyReadonly(e.key().clone()));
                    }
                    e.get_mut().value = Some(value);
                }
                e.get_mut().export = e.get_mut().export || add_export;
                e.get_mut().readonly = e.get_mut().readonly || add_readonly;
            }
            Entry::Vacant(e) => {
                e.insert(Value {
                    value,
                    export: add_export,
                    readonly: add_readonly,
                });
            }
        }
        Ok(())
    }

    pub fn get_str_value(&self, name: &str) -> Option<&str> {
        self.variables
            .get(name)
            .map(|val| val.value.as_deref())
            .flatten()
    }

    pub fn get_var(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut Value> {
        self.variables.get_mut(name)
    }

    pub fn unset(&mut self, name: &str) -> Result<(), CannotModifyReadonly> {
        if let Some(var) = self.variables.get_mut(name) {
            if var.readonly {
                return Err(CannotModifyReadonly(name.to_string()));
            }
            var.value = None;
        }
        Ok(())
    }
}

impl<I: IntoIterator<Item = (String, Value)>> From<I> for Environment {
    fn from(value: I) -> Self {
        Self {
            variables: value.into_iter().collect(),
        }
    }
}
