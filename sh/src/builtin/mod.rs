use crate::builtin::alias::AliasBuiltin;
use crate::builtin::cd::Cd;
use crate::builtin::control_flow::{Break, Continue, Return};
use crate::builtin::dot::Dot;
use crate::builtin::eval::Eval;
use crate::builtin::exec::Exec;
use crate::builtin::exit::Exit;
use crate::builtin::export::Export;
use crate::builtin::readonly::ReadOnly;
use crate::builtin::set::SetSpecialBuiltin;
use crate::builtin::shift::Shift;
use crate::builtin::times::Times;
use crate::builtin::trap::Trap;
use crate::builtin::unset::BuiltinUnset;
use crate::shell::environment::{CannotModifyReadonly, Environment};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::OsError;
use std::fmt::{Display, Formatter};

pub mod alias;
mod cd;
mod control_flow;
mod dot;
mod eval;
mod exec;
mod exit;
mod export;
mod readonly;
pub mod set;
mod shift;
mod times;
pub mod trap;
mod unset;

pub enum BuiltinError {
    CustomError(String),
    AssignmentError(CannotModifyReadonly),
    OsError(OsError),
}

impl From<CannotModifyReadonly> for BuiltinError {
    fn from(value: CannotModifyReadonly) -> Self {
        Self::AssignmentError(value)
    }
}

impl From<String> for BuiltinError {
    fn from(value: String) -> Self {
        Self::CustomError(value)
    }
}

impl From<&str> for BuiltinError {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

impl From<OsError> for BuiltinError {
    fn from(value: OsError) -> Self {
        Self::OsError(value)
    }
}

impl Display for BuiltinError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinError::CustomError(err) => f.write_str(err),
            BuiltinError::AssignmentError(err) => {
                write!(f, "{}", err)
            }
            BuiltinError::OsError(err) => {
                write!(f, "{}", err)
            }
        }
    }
}

pub type BuiltinResult = Result<i32, BuiltinError>;

pub trait SpecialBuiltinUtility {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult;
}

struct BuiltinNull;

impl SpecialBuiltinUtility for BuiltinNull {
    fn exec(&self, args: &[String], shell: &mut Shell, _: &mut OpenedFiles) -> BuiltinResult {
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
        "exit" => Some(&Exit),
        "export" => Some(&Export),
        "readonly" => Some(&ReadOnly),
        "return" => Some(&Return),
        "set" => Some(&SetSpecialBuiltin),
        "shift" => Some(&Shift),
        "times" => Some(&Times),
        "trap" => Some(&Trap),
        "unset" => Some(&BuiltinUnset),
        _ => None,
    }
}

pub trait BuiltinUtility {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
        environment: Environment,
    ) -> BuiltinResult;
}

pub fn get_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        "cd" => Some(&Cd),
        _ => None,
    }
}
