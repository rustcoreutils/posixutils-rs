//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::os::errno::get_current_errno_value;
use crate::shell::Shell;
use crate::shell::opened_files::OpenedFiles;
use std::fmt::Display;

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
    Number(libc::rlim_t),
}

impl LimitQuantity {
    fn parse(value: &str) -> Result<Self, ()> {
        if value == "unlimited" {
            Ok(LimitQuantity::Unlimited)
        } else {
            value
                .parse::<libc::rlim_t>()
                .map(LimitQuantity::Number)
                .map_err(|_| ())
        }
    }

    fn new(limit: libc::rlim_t, divisor: libc::rlim_t) -> Self {
        if limit == libc::RLIM_INFINITY {
            LimitQuantity::Unlimited
        } else {
            LimitQuantity::Number(limit / divisor)
        }
    }

    fn into_rlim_t(self, multiplier: libc::rlim_t) -> Result<libc::rlim_t, String> {
        match self {
            LimitQuantity::Unlimited => Ok(libc::RLIM_INFINITY),
            LimitQuantity::Number(value) => value
                .checked_mul(multiplier)
                .ok_or("ulimit: value too large".to_string()),
        }
    }
}

impl From<LimitQuantity> for libc::rlim_t {
    fn from(limit: LimitQuantity) -> Self {
        match limit {
            LimitQuantity::Unlimited => libc::RLIM_INFINITY,
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

        match (limit, resource_limit) {
            (None, Some(resource)) => {
                if newlimit.is_some() {
                    Ok(UlimitArgs {
                        limit: LimitType::Both,
                        resource,
                        newlimit,
                    })
                } else {
                    Ok(UlimitArgs {
                        limit: LimitType::Soft,
                        resource,
                        newlimit: None,
                    })
                }
            }
            (Some(lim), None) => Ok(UlimitArgs {
                limit: lim,
                resource: ResourceLimit::FSize,
                newlimit,
            }),
            (Some(lim), Some(resource)) => Ok(UlimitArgs {
                limit: lim,
                resource,
                newlimit,
            }),
            (None, None) => Err("ulimit: invalid arguments".to_string()),
        }
    }
}

fn get_limits(resource: libc::c_int) -> libc::rlimit {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    let result = unsafe { libc::getrlimit(resource as _, &mut limit) };
    if result < 0 {
        panic!("invalid call to getrlimit")
    }
    limit
}

fn get_limit(resource: libc::c_int, hard_limit: bool, divisor: libc::rlim_t) -> LimitQuantity {
    let limits = get_limits(resource);
    if hard_limit {
        LimitQuantity::new(limits.rlim_max, divisor)
    } else {
        LimitQuantity::new(limits.rlim_cur, divisor)
    }
}

fn set_limit(
    resource: libc::c_int,
    newlimit: LimitQuantity,
    limit_type: LimitType,
    multiplier: libc::rlim_t,
) -> Result<(), String> {
    let mut limits = get_limits(resource);
    let newlimit = newlimit.into_rlim_t(multiplier)?;
    match limit_type {
        LimitType::Soft => {
            limits.rlim_cur = newlimit;
            limits.rlim_max = limits.rlim_max.max(newlimit);
        }
        LimitType::Hard => {
            limits.rlim_max = newlimit;
        }
        LimitType::Both => {
            limits.rlim_cur = newlimit;
            limits.rlim_max = newlimit;
        }
    }
    let result = unsafe { libc::setrlimit(resource as _, &limits) };
    if result < 0 {
        return Err(format!(
            "ulimit: cannot modify limit ({})",
            get_current_errno_value()
        ));
    }
    Ok(())
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
                    set_limit(libc::RLIMIT_CORE as libc::c_int, newlimit, args.limit, 512)?;
                }
                ResourceLimit::Data => {
                    set_limit(libc::RLIMIT_DATA as libc::c_int, newlimit, args.limit, 1024)?;
                }
                ResourceLimit::FSize => {
                    set_limit(libc::RLIMIT_FSIZE as libc::c_int, newlimit, args.limit, 512)?;
                }
                ResourceLimit::NoFile => {
                    set_limit(libc::RLIMIT_NOFILE as libc::c_int, newlimit, args.limit, 1)?;
                }
                ResourceLimit::Stack => {
                    set_limit(
                        libc::RLIMIT_STACK as libc::c_int,
                        newlimit,
                        args.limit,
                        1024,
                    )?;
                }
                ResourceLimit::Cpu => {
                    set_limit(libc::RLIMIT_CPU as libc::c_int, newlimit, args.limit, 1)?;
                }
                ResourceLimit::As => {
                    set_limit(libc::RLIMIT_AS as libc::c_int, newlimit, args.limit, 1024)?;
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
                    get_limit(libc::RLIMIT_CORE as libc::c_int, hard_limit, 512)
                ));
            }
            if args.resource == ResourceLimit::Data || all {
                opened_files.write_out(format!(
                    "data seg size (-d)                 [KiB] {}\n",
                    get_limit(libc::RLIMIT_DATA as libc::c_int, hard_limit, 1024)
                ));
            }
            if args.resource == ResourceLimit::FSize || all {
                opened_files.write_out(format!(
                    "file size (-f)                  [blocks] {}\n",
                    get_limit(libc::RLIMIT_FSIZE as libc::c_int, hard_limit, 512)
                ));
            }
            if args.resource == ResourceLimit::NoFile || all {
                opened_files.write_out(format!(
                    "open files (-n)                          {}\n",
                    get_limit(libc::RLIMIT_NOFILE as libc::c_int, hard_limit, 1)
                ));
            }
            if args.resource == ResourceLimit::Stack || all {
                opened_files.write_out(format!(
                    "stack size (-s)                    [KiB] {}\n",
                    get_limit(libc::RLIMIT_STACK as libc::c_int, hard_limit, 1024)
                ));
            }
            if args.resource == ResourceLimit::Cpu || all {
                opened_files.write_out(format!(
                    "cpu time (-t)                  [seconds] {}\n",
                    get_limit(libc::RLIMIT_CPU as libc::c_int, hard_limit, 1)
                ));
            }
            if args.resource == ResourceLimit::As || all {
                opened_files.write_out(format!(
                    "virtual memory (-v)                [KiB] {}\n",
                    get_limit(libc::RLIMIT_AS as libc::c_int, hard_limit, 1024)
                ));
            }
        }

        Ok(0)
    }
}
