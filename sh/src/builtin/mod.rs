use crate::builtin::alias::AliasBuiltin;
use crate::builtin::cd::Cd;
use crate::builtin::command::Command;
use crate::builtin::control_flow::{Break, Continue, Return};
use crate::builtin::dot::Dot;
use crate::builtin::eval::Eval;
use crate::builtin::exec::Exec;
use crate::builtin::exit::Exit;
use crate::builtin::export::Export;
use crate::builtin::fc::Fc;
use crate::builtin::getopts::GetOpts;
use crate::builtin::jobs::Jobs;
use crate::builtin::kill::Kill;
use crate::builtin::readonly::ReadOnly;
use crate::builtin::set::SetSpecialBuiltin;
use crate::builtin::shift::Shift;
use crate::builtin::times::Times;
use crate::builtin::trap::Trap;
use crate::builtin::type_::Type_;
use crate::builtin::ulimit::Ulimit;
use crate::builtin::unalias::Unalias;
use crate::builtin::unset::BuiltinUnset;
use crate::builtin::wait::Wait;
use crate::jobs::parse_job_id;
use crate::shell::variables::{CannotModifyReadonly, Variables};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::OsError;
use nix::libc::pid_t;
use nix::unistd::Pid;
use std::fmt::{Display, Formatter};

pub mod alias;
mod cd;
mod command;
mod control_flow;
mod dot;
mod eval;
mod exec;
mod exit;
mod export;
mod fc;
mod getopts;
mod jobs;
mod kill;
mod readonly;
pub mod set;
mod shift;
mod times;
pub mod trap;
mod type_;
mod ulimit;
mod unalias;
mod unset;
mod wait;

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
    ) -> BuiltinResult;
}

pub fn get_builtin_utility(name: &str) -> Option<&dyn BuiltinUtility> {
    match name {
        "alias" => Some(&AliasBuiltin),
        "command" => Some(&Command),
        "getopts" => Some(&GetOpts),
        "kill" => Some(&Kill),
        "ulimit" => Some(&Ulimit),
        "wait" => Some(&Wait),
        "fc" => Some(&Fc),
        "cd" => Some(&Cd),
        "jobs" => Some(&Jobs),
        "type" => Some(&Type_),
        "unalias" => Some(&Unalias),
        _ => None,
    }
}

fn skip_option_terminator(args: &[String]) -> &[String] {
    if args.get(0).is_some_and(|arg| arg == "--") {
        &args[1..]
    } else {
        &args
    }
}

fn parse_pid(pid: &str, shell: &Shell) -> Result<Pid, String> {
    if pid.starts_with('%') {
        let job_id = parse_job_id(pid).map_err(|_| format!("'{pid}' is not a valid job id"))?;
        let job = shell
            .get_job(job_id)
            .ok_or(format!("'{pid}' no such job"))?;
        Ok(job.pid)
    } else {
        let raw_pid = pid
            .parse::<pid_t>()
            .map_err(|_| format!("'{pid}' is not a valid pid"))?;
        Ok(Pid::from_raw(raw_pid))
    }
}
