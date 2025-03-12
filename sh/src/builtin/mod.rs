use crate::builtin::alias::AliasBuiltin;
use crate::builtin::cd::Cd;
use crate::builtin::control_flow::{Break, Continue};
use crate::builtin::dot::Dot;
use crate::builtin::eval::Eval;
use crate::builtin::exec::Exec;
use crate::builtin::readonly::ReadOnly;
use crate::builtin::set::SetSpecialBuiltin;
use crate::builtin::unset::BuiltinUnset;
use crate::shell::environment::Environment;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

pub mod alias;
mod cd;
mod control_flow;
mod dot;
mod eval;
mod exec;
mod readonly;
pub mod set;
mod unset;

pub type SpecialBuiltinResult = Result<i32, String>;

pub trait SpecialBuiltinUtility {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> SpecialBuiltinResult;
}

struct BuiltinNull;

impl SpecialBuiltinUtility for BuiltinNull {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        _: &mut OpenedFiles,
    ) -> SpecialBuiltinResult {
        Ok(0)
    }
}

pub fn get_special_builtin_utility(name: &str) -> Option<&dyn SpecialBuiltinUtility> {
    match name {
        "break" => Some(&Break),
        ":" => Some(&BuiltinNull),
        "continue" => Some(&Continue),
        "." => Some(&Dot),
        "eval" => Some(&Eval),
        "exec" => Some(&Exec),
        "set" => Some(&SetSpecialBuiltin),
        "readonly" => Some(&ReadOnly),
        "unset" => Some(&BuiltinUnset),
        _ => None,
    }
}

pub trait BuiltinUtility {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: OpenedFiles,
        environment: Environment,
    ) -> i32;
}

pub fn get_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        "cd" => Some(&Cd),
        _ => None,
    }
}
