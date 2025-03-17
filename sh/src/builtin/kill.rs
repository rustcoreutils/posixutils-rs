use crate::builtin::{skip_option_terminator, BuiltinResult, BuiltinUtility};
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::signals::{Signal, SIGNALS};
use nix::libc::pid_t;
use nix::sys::signal::kill;
use nix::sys::signal::Signal as NixSignal;
use nix::unistd::Pid;
use std::str::FromStr;

enum KillArgs<'a> {
    SendSignal {
        signal: Option<Signal>,
        pids: &'a [String],
    },
    ListSignals,
}

fn get_pids(args: &[String]) -> Result<&[String], String> {
    let pids = skip_option_terminator(&args);
    if pids.is_empty() {
        return Err("kill: missing operand".to_string());
    }
    Ok(pids)
}

impl<'a> KillArgs<'a> {
    fn parse(args: &'a [String]) -> Result<Self, String> {
        if args.is_empty() {
            return Err("kill: missing operand".to_string());
        }

        if args.len() == 1 && args[0] == "-l" {
            return Ok(Self::ListSignals);
        }

        if args[0] == "-s" && args.len() > 2 {
            let signal = Signal::from_str(&args[1])
                .map_err(|_| format!("kill: invalid signal '{}'", args[1]))?;
            let pids = get_pids(&args[2..])?;
            return Ok(KillArgs::SendSignal {
                pids,
                signal: Some(signal),
            });
        }

        if args[0].starts_with('-') && &args[0] != "--" {
            let signal = if &args[0][1..] == "0" {
                None
            } else {
                Some(
                    Signal::from_str(&args[0][1..])
                        .map_err(|_| format!("kill: invalid signal '{}'", args[1]))?,
                )
            };
            let pids = get_pids(&args[1..])?;
            return Ok(KillArgs::SendSignal { pids, signal });
        }

        let pids = get_pids(&args)?;
        Ok(KillArgs::SendSignal {
            pids,
            signal: Some(Signal::SigTerm),
        })
    }
}

pub struct Kill;

impl BuiltinUtility for Kill {
    fn exec(
        &self,
        args: &[String],
        _: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        // TODO: handle job ids
        // TODO: handle -l with op
        let args = KillArgs::parse(args)?;

        match args {
            KillArgs::SendSignal { signal, pids } => {
                for pid in pids {
                    let pid = pid
                        .parse::<pid_t>()
                        .map_err(|_| format!("kill: '{pid}' is not a valid pid"))?;
                    kill(Pid::from_raw(pid), signal.map(|s| NixSignal::from(s)))
                        .map_err(|err| format!("kill: failed to send signal ({})", err))?;
                }
            }
            KillArgs::ListSignals => {
                for signal in SIGNALS {
                    opened_files.write_out(format!("{}\n", signal));
                }
            }
        }

        Ok(0)
    }
}
