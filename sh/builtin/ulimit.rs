//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::builtin::{BuiltinResult, BuiltinUtility};
use crate::option_parser::OptionParser;
use crate::os::errno::get_current_errno_value;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
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
            // POSIX: with no options other than -H/-S, -f is implied, and
            // without a newlimit operand the default is -S.
            return Ok(UlimitArgs {
                limit: LimitType::Soft,
                resource: ResourceLimit::FSize,
                newlimit: None,
            });
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
            // POSIX synopsis is `ulimit [-f] [blocks]`: with no resource
            // option, -f is implied, and a newlimit with neither -H nor -S
            // sets both limits.
            (None, None) => Ok(UlimitArgs {
                limit: if newlimit.is_some() {
                    LimitType::Both
                } else {
                    LimitType::Soft
                },
                resource: ResourceLimit::FSize,
                newlimit,
            }),
        }
    }
}

fn get_limits(resource: libc::c_int) -> Result<libc::rlimit, String> {
    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    let result = unsafe { libc::getrlimit(resource as _, &mut limit) };
    if result < 0 {
        return Err(format!(
            "ulimit: cannot read limit ({})",
            get_current_errno_value()
        ));
    }
    Ok(limit)
}

fn get_limit(
    resource: libc::c_int,
    hard_limit: bool,
    divisor: libc::rlim_t,
) -> Result<LimitQuantity, String> {
    let limits = get_limits(resource)?;
    if hard_limit {
        Ok(LimitQuantity::new(limits.rlim_max, divisor))
    } else {
        Ok(LimitQuantity::new(limits.rlim_cur, divisor))
    }
}

fn set_limit(
    resource: libc::c_int,
    newlimit: LimitQuantity,
    limit_type: LimitType,
    multiplier: libc::rlim_t,
) -> Result<(), String> {
    let mut limits = get_limits(resource)?;
    let newlimit = newlimit.into_rlim_t(multiplier)?;
    match limit_type {
        LimitType::Soft => {
            // Only a privileged process may raise the hard limit, and `-S`
            // asks for the soft limit alone; refuse rather than silently
            // widening the hard limit.
            if limits.rlim_max != libc::RLIM_INFINITY
                && (newlimit == libc::RLIM_INFINITY || newlimit > limits.rlim_max)
            {
                return Err("ulimit: soft limit above hard limit".to_string());
            }
            limits.rlim_cur = newlimit;
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

/// A resource `ulimit` can report, with the labelled form used by `-a`.
struct ResourceInfo {
    resource: ResourceLimit,
    libc_resource: libc::c_int,
    /// short phrase, units and option letter, as `-a` prints them
    description: &'static str,
    /// native units per reported unit
    divisor: libc::rlim_t,
}

const RESOURCES: &[ResourceInfo] = &[
    ResourceInfo {
        resource: ResourceLimit::Core,
        libc_resource: libc::RLIMIT_CORE as libc::c_int,
        description: "core file size (-c)             [blocks]",
        divisor: 512,
    },
    ResourceInfo {
        resource: ResourceLimit::Data,
        libc_resource: libc::RLIMIT_DATA as libc::c_int,
        description: "data seg size (-d)                 [KiB]",
        divisor: 1024,
    },
    ResourceInfo {
        resource: ResourceLimit::FSize,
        libc_resource: libc::RLIMIT_FSIZE as libc::c_int,
        description: "file size (-f)                  [blocks]",
        divisor: 512,
    },
    ResourceInfo {
        resource: ResourceLimit::NoFile,
        libc_resource: libc::RLIMIT_NOFILE as libc::c_int,
        description: "open files (-n)                         ",
        divisor: 1,
    },
    ResourceInfo {
        resource: ResourceLimit::Stack,
        libc_resource: libc::RLIMIT_STACK as libc::c_int,
        description: "stack size (-s)                    [KiB]",
        divisor: 1024,
    },
    ResourceInfo {
        resource: ResourceLimit::Cpu,
        libc_resource: libc::RLIMIT_CPU as libc::c_int,
        description: "cpu time (-t)                  [seconds]",
        divisor: 1,
    },
    ResourceInfo {
        resource: ResourceLimit::As,
        libc_resource: libc::RLIMIT_AS as libc::c_int,
        description: "virtual memory (-v)                [KiB]",
        divisor: 1024,
    },
];

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
            let hard_limit = args.limit == LimitType::Hard;
            if args.resource == ResourceLimit::All {
                for info in RESOURCES {
                    opened_files.write_out(format!(
                        "{} {}\n",
                        info.description,
                        get_limit(info.libc_resource, hard_limit, info.divisor)?
                    ));
                }
            } else {
                // POSIX: a single limit value is written as "%1d\n", or
                // "unlimited\n" when the resource has no numeric limit.
                let info = RESOURCES
                    .iter()
                    .find(|info| info.resource == args.resource)
                    .ok_or_else(|| "ulimit: unknown resource".to_string())?;
                opened_files.write_out(format!(
                    "{}\n",
                    get_limit(info.libc_resource, hard_limit, info.divisor)?
                ));
            }
        }

        Ok(0)
    }
}
