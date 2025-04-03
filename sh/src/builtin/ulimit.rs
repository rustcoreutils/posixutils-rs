//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt::Display;

use nix::libc::{rlim_t, RLIM_INFINITY};
use nix::sys::resource::Resource;

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ResourceLimit {
    All,
    Core,
    Data,
    FSize,
    NoFile,
    Stack,
    Cpu,
    As,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum LimitType {
    Soft,
    Hard,
    Both,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum LimitQuantity {
    Unlimited,
    Number(rlim_t),
}

impl LimitQuantity {
    fn parse(value: &str) -> Result<Self, ()> {
        if value == "unlimited" {
            Ok(LimitQuantity::Unlimited)
        } else {
            value
                .parse::<rlim_t>()
                .map(LimitQuantity::Number)
                .map_err(|_| ())
        }
    }

    fn new(limit: rlim_t, divisor: rlim_t) -> Self {
        if limit == RLIM_INFINITY {
            LimitQuantity::Unlimited
        } else {
            LimitQuantity::Number(limit / divisor)
        }
    }

    fn into_rlim_t(self, multiplier: rlim_t) -> Result<rlim_t, String> {
        match self {
            LimitQuantity::Unlimited => Ok(RLIM_INFINITY),
            LimitQuantity::Number(value) => value
                .checked_mul(multiplier)
                .ok_or("ulimit: value too large".to_string()),
        }
    }
}

impl From<LimitQuantity> for rlim_t {
    fn from(limit: LimitQuantity) -> Self {
        match limit {
            LimitQuantity::Unlimited => RLIM_INFINITY,
            LimitQuantity::Number(value) => value,
        }
    }
}

impl Display for LimitQuantity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LimitQuantity::Unlimited => write!(f, "unlimited"),
            LimitQuantity::Number(value) => write!(f, "{}", value),
        }
    }
}

struct UlimitArgs {
    limit: LimitType,
    resource: ResourceLimit,
    newlimit: Option<LimitQuantity>,
}

impl UlimitArgs {
    fn parse(args: &[String]) -> Result<Self, String> {
        if args.is_empty() {
            return Err("ulimit: missing argument".to_string());
        }
        let mut option_parser = OptionParser::new(args);
        let mut limit = None;
        let mut resource_limit = None;
        while let Some(option) = option_parser
            .next_option()
            .map_err(|opt| format!("ulimit: invalid option {opt}"))?
        {
            match option {
                'H' => limit = Some(LimitType::Hard),
                'S' => limit = Some(LimitType::Soft),
                other => {
                    if resource_limit.is_some() {
                        return Err("ulimit: too many arguments".to_string());
                    }
                    match other {
                        'a' => resource_limit = Some(ResourceLimit::All),
                        'c' => resource_limit = Some(ResourceLimit::Core),
                        'd' => resource_limit = Some(ResourceLimit::Data),
                        'f' => resource_limit = Some(ResourceLimit::FSize),
                        'n' => resource_limit = Some(ResourceLimit::NoFile),
                        's' => resource_limit = Some(ResourceLimit::Stack),
                        't' => resource_limit = Some(ResourceLimit::Cpu),
                        'v' => resource_limit = Some(ResourceLimit::As),
                        _ => return Err(format!("ulimit: invalid option {}", option)),
                    }
                }
            }
        }
        let first_operant = option_parser.next_argument();
        let newlimit = if let Some(arg) = args.get(first_operant) {
            Some(
                LimitQuantity::parse(arg)
                    .map_err(|_| format!("ulimit: '{arg}' is not a valid limit"))?,
            )
        } else {
            None
        };

        if first_operant < args.len() - 1 {
            return Err("ulimit: too many arguments".to_string());
        }

        if resource_limit.is_some_and(|limit| limit == ResourceLimit::All) && newlimit.is_some() {
            return Err("ulimit: cannot set limit for all resources".to_string());
        }

        if limit.is_none() && resource_limit.is_some() {
            if newlimit.is_some() {
                return Ok(UlimitArgs {
                    limit: LimitType::Both,
                    resource: resource_limit.unwrap(),
                    newlimit,
                });
            }
            return Ok(UlimitArgs {
                limit: LimitType::Soft,
                resource: resource_limit.unwrap(),
                newlimit: None,
            });
        }

        if limit.is_some() && resource_limit.is_none() {
            return Ok(UlimitArgs {
                limit: limit.unwrap(),
                resource: ResourceLimit::FSize,
                newlimit,
            });
        }

        if limit.is_some() && resource_limit.is_some() {
            return Ok(UlimitArgs {
                limit: limit.unwrap(),
                resource: resource_limit.unwrap(),
                newlimit,
            });
        }

        Err("ulimit: invalid arguments".to_string())
    }
}

fn get_limit(resource: Resource, hard_limit: bool, divisor: rlim_t) -> LimitQuantity {
    // from what I can tell, this function can never fail, so unwrap is safe
    let (soft, hard) = nix::sys::resource::getrlimit(resource).unwrap();
    if hard_limit {
        LimitQuantity::new(hard, divisor)
    } else {
        LimitQuantity::new(soft, divisor)
    }
}

fn set_limit(
    resource: Resource,
    newlimit: LimitQuantity,
    limit_type: LimitType,
    multiplier: rlim_t,
) -> Result<(), String> {
    fn map_err(err: nix::Error) -> String {
        format!("ulimit: cannot modify limit ({err})")
    }

    let (prev_soft, prev_hard) = nix::sys::resource::getrlimit(resource).unwrap();
    let newlimit = newlimit.into_rlim_t(multiplier)?;
    match limit_type {
        LimitType::Soft => {
            nix::sys::resource::setrlimit(resource, newlimit, prev_hard).map_err(map_err)
        }
        LimitType::Hard => {
            nix::sys::resource::setrlimit(resource, prev_soft, newlimit).map_err(map_err)
        }
        LimitType::Both => {
            nix::sys::resource::setrlimit(resource, newlimit, newlimit).map_err(map_err)
        }
    }
}

pub struct Ulimit;

impl BuiltinUtility for Ulimit {
    fn exec(
        &self,
        args: &[String],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let args = UlimitArgs::parse(args)?;

        if let Some(newlimit) = args.newlimit {
            match args.resource {
                ResourceLimit::Core => {
                    set_limit(Resource::RLIMIT_CORE, newlimit, args.limit, 512)?;
                }
                ResourceLimit::Data => {
                    set_limit(Resource::RLIMIT_DATA, newlimit, args.limit, 1024)?;
                }
                ResourceLimit::FSize => {
                    set_limit(Resource::RLIMIT_FSIZE, newlimit, args.limit, 512)?;
                }
                ResourceLimit::NoFile => {
                    set_limit(Resource::RLIMIT_NOFILE, newlimit, args.limit, 1)?;
                }
                ResourceLimit::Stack => {
                    set_limit(Resource::RLIMIT_STACK, newlimit, args.limit, 1024)?;
                }
                ResourceLimit::Cpu => {
                    set_limit(Resource::RLIMIT_CPU, newlimit, args.limit, 1)?;
                }
                ResourceLimit::As => {
                    set_limit(Resource::RLIMIT_AS, newlimit, args.limit, 1024)?;
                }
                ResourceLimit::All => unreachable!(),
            }
        } else {
            assert_ne!(args.limit, LimitType::Both);
            let all = args.resource == ResourceLimit::All;
            let hard_limit = args.limit == LimitType::Hard;
            if args.resource == ResourceLimit::Core || all {
                opened_files.write_out(format!(
                    "core file size (-c)             [blocks] {}\n",
                    get_limit(Resource::RLIMIT_CORE, hard_limit, 512)
                ));
            }
            if args.resource == ResourceLimit::Data || all {
                opened_files.write_out(format!(
                    "data seg size (-d)                 [KiB] {}\n",
                    get_limit(Resource::RLIMIT_DATA, hard_limit, 1024)
                ));
            }
            if args.resource == ResourceLimit::FSize || all {
                opened_files.write_out(format!(
                    "file size (-f)                  [blocks] {}\n",
                    get_limit(Resource::RLIMIT_FSIZE, hard_limit, 512)
                ));
            }
            if args.resource == ResourceLimit::NoFile || all {
                opened_files.write_out(format!(
                    "open files (-n)                          {}\n",
                    get_limit(Resource::RLIMIT_NOFILE, hard_limit, 1)
                ));
            }
            if args.resource == ResourceLimit::Stack || all {
                opened_files.write_out(format!(
                    "stack size (-s)                    [KiB] {}\n",
                    get_limit(Resource::RLIMIT_STACK, hard_limit, 1024)
                ));
            }
            if args.resource == ResourceLimit::Cpu || all {
                opened_files.write_out(format!(
                    "cpu time (-t)                  [seconds] {}\n",
                    get_limit(Resource::RLIMIT_CPU, hard_limit, 1)
                ));
            }
            if args.resource == ResourceLimit::As || all {
                opened_files.write_out(format!(
                    "virtual memory (-v)                [KiB] {}\n",
                    get_limit(Resource::RLIMIT_AS, hard_limit, 1024)
                ));
            }
        }

        Ok(0)
    }
}
