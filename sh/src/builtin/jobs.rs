use crate::builtin::{BuiltinError, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobState};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;
use crate::utils::{waitpid, OsError};
use nix::sys::wait::{WaitPidFlag, WaitStatus};
use nix::unistd::Pid;

#[derive(PartialEq, Eq, Clone, Copy)]
enum PrintOptions {
    Long,
    Default,
    Short,
}

fn job_state(pid: Pid) -> Result<String, OsError> {
    match waitpid(pid, Some(WaitPidFlag::WNOHANG))? {
        WaitStatus::Exited(_, status) => {
            if status == 0 {
                Ok("Done".to_string())
            } else {
                Ok(format!("Done({status})"))
            }
        }
        WaitStatus::Signaled(_, _, _) => {
            todo!()
        }
        WaitStatus::Stopped(_, _) => {
            todo!()
        }
        WaitStatus::StillAlive => Ok("Running".to_string()),
        _ => unreachable!(),
    }
}

fn print_job(
    job: &Job,
    print_option: PrintOptions,
    opened_files: &mut OpenedFiles,
) -> Result<(), BuiltinError> {
    match print_option {
        PrintOptions::Default | PrintOptions::Long => {
            let current = if job.state == JobState::Current {
                "+"
            } else if job.state == JobState::Previous {
                "-"
            } else {
                " "
            };
            let state = job_state(job.pid)?;
            if print_option == PrintOptions::Long {
                opened_files.write_out(format!(
                    "[{}]{} {} {} {:>20}\n",
                    job.number, current, job.pid, state, job.command
                ));
            } else {
                opened_files.write_out(format!(
                    "[{}]{} {} {:>20}\n",
                    job.number, current, state, job.command
                ));
            }
        }
        PrintOptions::Short => {
            opened_files.write_out(format!("{}\n", job.pid));
        }
    }
    Ok(())
}

pub struct Jobs;

impl BuiltinUtility for Jobs {
    fn exec(
        &self,
        args: &[String],
        shell: &mut Shell,
        opened_files: &mut OpenedFiles,
    ) -> BuiltinResult {
        let mut print_option = PrintOptions::Default;
        let mut options_parser = OptionParser::new(args);
        while let Some(option) = options_parser
            .next_option()
            .map_err(|arg| format!("jobs: invalid option: {arg}"))?
        {
            if option == 'l' || option == 'p' {
                if print_option != PrintOptions::Default {
                    return Err("jobs: can only specify either -l or -p".into());
                }
                print_option = if option == 'l' {
                    PrintOptions::Long
                } else {
                    PrintOptions::Short
                };
            } else {
                return Err(format!("jobs: invalid option -{option}").into());
            }
        }

        if options_parser.next_argument() == args.len() {
            for job in &shell.background_jobs {
                print_job(job, print_option, opened_files)?;
            }
        } else {
            for operand in &args[options_parser.next_argument()..] {
                let job_id = parse_job_id(operand)
                    .map_err(|_| format!("jobs: invalid job id '{operand}'"))?;
                if let Some(job) = shell.get_job(job_id) {
                    print_job(job, print_option, opened_files)?;
                } else {
                    return Err(format!("jobs: '{operand}' no such job").into());
                }
            }
        }

        Ok(0)
    }
}
