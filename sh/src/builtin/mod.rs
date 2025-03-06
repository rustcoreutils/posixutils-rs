use crate::builtin::alias::AliasBuiltin;
use crate::builtin::control_flow::{Break, Continue};
use crate::builtin::readonly::ReadOnly;
use crate::builtin::set::SetSpecialBuiltin;
use crate::builtin::unset::BuiltinUnset;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use std::io::Write;

pub mod alias;
mod control_flow;
mod readonly;
pub mod set;
mod unset;

pub trait SpecialBuiltinUtility {
    fn exec(&self, args: &[String], shell: &mut Shell, opened_files: OpenedFiles) -> i32;
}

struct BuiltinNull;

impl SpecialBuiltinUtility for BuiltinNull {
    fn exec(&self, _: &[String], _: &mut Shell, _: OpenedFiles) -> i32 {
        0
    }
}

pub fn get_special_builtin_utility(name: &str) -> Option<&dyn SpecialBuiltinUtility> {
    match name {
        "set" => Some(&SetSpecialBuiltin),
        "readonly" => Some(&ReadOnly),
        "break" => Some(&Break),
        "continue" => Some(&Continue),
        ":" => Some(&BuiltinNull),
        "unset" => Some(&BuiltinUnset),
        _ => None,
    }
}

pub trait BuiltinUtility {
    fn exec(&self, args: &[String], shell: &mut Shell) -> i32;
}

pub fn get_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        _ => None,
    }
}
