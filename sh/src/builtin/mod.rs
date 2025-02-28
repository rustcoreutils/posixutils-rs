use crate::builtin::alias::AliasBuiltin;
use crate::builtin::readonly::ReadOnly;
use crate::builtin::set::SetSpecialBuiltin;
use crate::shell::Shell;

pub mod alias;
mod readonly;
pub mod set;

pub trait BuiltinUtility {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32;
}

pub fn get_special_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "set" => Some(&SetSpecialBuiltin),
        "readonly" => Some(&ReadOnly),
        _ => None,
    }
}

pub fn get_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        _ => None,
    }
}
