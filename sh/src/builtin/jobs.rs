use crate::builtin::{BuiltinError, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job, JobPosition, JobState};
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

fn job_state_to_string(state: JobState) -> String {
    match state {
        JobState::Done(code) => {
            if code == 0 {
                "Done".to_string()
            } else {
                format!("Done({})", code)
            }
        }
        JobState::Running => "Running".to_string(),
        JobState::Stopped => "Stopped".to_string(),
    }
}

fn print_job(
    job: &Job,
    print_option: PrintOptions,
    opened_files: &mut OpenedFiles,
) -> Result<(), BuiltinError> {
    match print_option {
        PrintOptions::Default | PrintOptions::Long => {
            let current = if job.position == JobPosition::Current {
                "+"
            } else if job.position == JobPosition::Previous {
                "-"
            } else {
                " "
            };
            let state = job_state_to_string(job.state);
            if print_option == PrintOptions::Long {
                opened_files.write_out(format!(
                    "[{}]{} {} {} {}\n",
                    job.number, current, job.pid, state, job.command
                ));
            } else {
                opened_files.write_out(format!(
                    "[{}]{} {} {}\n",
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
            for job in shell.background_jobs.iter() {
                print_job(job, print_option, opened_files)?;
            }
        } else {
            for operand in &args[options_parser.next_argument()..] {
                let job_id = parse_job_id(operand)
                    .map_err(|_| format!("jobs: invalid job id '{operand}'"))?;
                if let Some(job) = shell.background_jobs.get_job(job_id) {
                    print_job(job, print_option, opened_files)?;
                } else {
                    return Err(format!("jobs: '{operand}' no such job").into());
                }
            }
        }

        Ok(0)
    }
}
