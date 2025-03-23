use crate::builtin::{BuiltinError, BuiltinResult, BuiltinUtility};
use crate::jobs::{parse_job_id, Job};
use crate::option_parser::OptionParser;
use crate::shell::opened_files::OpenedFiles;
use crate::shell::Shell;

#[derive(PartialEq, Eq, Clone, Copy)]
enum PrintOptions {
    Long,
    Default,
    Short,
}

fn print_job(
    job: &Job,
    print_option: PrintOptions,
    opened_files: &mut OpenedFiles,
) -> Result<(), BuiltinError> {
    match print_option {
        PrintOptions::Default | PrintOptions::Long => {
            if print_option == PrintOptions::Long {
                opened_files.write_out(job.to_string_long());
            } else {
                opened_files.write_out(job.to_string_short());
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
