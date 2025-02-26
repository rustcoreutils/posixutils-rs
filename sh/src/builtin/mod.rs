use crate::builtin::alias::AliasBuiltin;
use crate::builtin::set::SetSpecialBuiltin;
use crate::shell::Shell;

pub mod alias;
pub mod set;

pub trait BuiltinUtility {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32;
}

pub fn get_special_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "set" => Some(&SetSpecialBuiltin),
        _ => None,
    }
}

pub fn get_bultin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        _ => None,
    }
}
