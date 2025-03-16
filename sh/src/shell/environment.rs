use crate::shell::Display;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;

#[derive(Clone, Default)]
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

    pub fn export_or(&mut self, value: bool) {
        self.export = self.export || value;
    }
}

pub type GlobalScope = HashMap<String, Value>;
pub type LocalScope = HashMap<String, String>;

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
    pub fn set_global(
        &mut self,
        name: String,
        value: String,
    ) -> Result<&mut Value, CannotModifyReadonly> {
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

    pub fn set_global_forced(&mut self, name: String, value: String) -> &mut Value {
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

    pub fn set(&mut self, name: String, value: String) -> Result<(), CannotModifyReadonly> {
        if let Some(innermost_scope) = self.local_scopes.last_mut() {
            innermost_scope.insert(name, value);
        } else {
            self.set_global(name, value)?;
        }
        Ok(())
    }

    pub fn get_str_value(&self, name: &str) -> Option<&str> {
        for local_scope in self.local_scopes.iter().rev() {
            if let Some(value) = local_scope.get(name).map(|val| val.as_str()) {
                return Some(value);
            }
        }
        self.global_scope
            .get(name)
            .map(|val| val.value.as_deref())
            .flatten()
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

    pub fn exported(&self) -> impl Iterator<Item = (&String, &String)> {
        let mut exported = HashSet::new();
        for (name, var) in &self.global_scope {
            if var.export {
                if let Some(value) = &var.value {
                    exported.insert((name, value));
                }
            }
        }
        for local_scope in &self.local_scopes {
            for (name, value) in local_scope {
                exported.insert((name, value));
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
