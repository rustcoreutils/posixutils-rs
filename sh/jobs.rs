//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::utils::{signal_to_exit_status, waitpid, OsResult};
use nix::sys::wait::{WaitPidFlag, WaitStatus};
use nix::unistd::Pid;
use std::fmt::{Display, Formatter, Write};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum JobPosition {
    Current,
    Previous,
    Other,
}

impl Display for JobPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JobPosition::Current => f.write_char('+'),
            JobPosition::Previous => f.write_char('-'),
            JobPosition::Other => f.write_char(' '),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum JobState {
    Done(i32),
    Running,
    Stopped,
}

impl Display for JobState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JobState::Done(code) => {
                if *code == 0 {
                    f.write_str("Done")
                } else {
                    write!(f, "Done({})", code)
                }
            }
            JobState::Running => f.write_str("Running"),
            JobState::Stopped => f.write_str("Stopped"),
        }
    }
}

#[derive(Clone)]
pub struct Job {
    pub command: String,
    pub pid: Pid,
    pub number: u64,
    pub position: JobPosition,
    pub state: JobState,
    pub state_should_be_reported: bool,
}

impl Job {
    pub fn to_string_long(&self) -> String {
        format!(
            "[{}]{} {} {}    {}\n",
            self.number, self.position, self.pid, self.state, self.command
        )
    }

    pub fn to_string_short(&self) -> String {
        format!(
            "[{}]{} {}    {}\n",
            self.number, self.position, self.state, self.command
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JobId<'s> {
    CurrentJob,
    PreviousJob,
    JobNumber(u64),
    BeginsWith(&'s str),
    Contains(&'s str),
}

/// # Panics
/// panics if text does not begin with '%'
pub fn parse_job_id(text: &str) -> Result<JobId, ()> {
    assert!(text.starts_with('%'));
    match &text[1..] {
        "%" | "+" => Ok(JobId::CurrentJob),
        n if n.chars().all(|c| c.is_ascii_digit()) => {
            let n = n.parse().map_err(|_| {})?;
            Ok(JobId::JobNumber(n))
        }
        "-" => Ok(JobId::PreviousJob),
        other if other.starts_with('?') => {
            if text.len() < 3 {
                Err(())
            } else {
                Ok(JobId::Contains(&text[2..]))
            }
        }
        other => {
            if other.is_empty() {
                Err(())
            } else {
                Ok(JobId::BeginsWith(other))
            }
        }
    }
}

#[derive(Clone)]
pub struct JobManager {
    jobs: Vec<Job>,
    last_job_number: u64,
}

impl JobManager {
    fn update_positions(&mut self) {
        for job in self.jobs.iter_mut().rev() {
            // `jobs` is always ordered, so we only have to check until
            // `position` is not `Current` or `Previous`
            if job.position == JobPosition::Other {
                break;
            }
            job.position = JobPosition::Other;
        }
        if let Some(job) = self.jobs.last_mut() {
            job.position = JobPosition::Current;
        }
        if self.jobs.len() > 1 {
            let len = self.jobs.len();
            self.jobs[len - 2].position = JobPosition::Previous;
        }
    }

    pub fn update_jobs(&mut self) -> OsResult<()> {
        for job in &mut self.jobs {
            if let JobState::Done(_) = job.state {
                continue;
            }
            match waitpid(job.pid, Some(WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED))? {
                WaitStatus::Exited(_, status) => {
                    job.state = JobState::Done(status);
                    job.state_should_be_reported = true;
                }
                WaitStatus::Signaled(_, signal, _) => {
                    if signal == nix::sys::signal::SIGTSTP {
                        job.state = JobState::Stopped;
                    } else {
                        job.state = JobState::Done(signal_to_exit_status(signal));
                    }
                    job.state_should_be_reported = true;
                }
                WaitStatus::StillAlive => {}
                WaitStatus::Stopped(_, _) => {
                    job.state = JobState::Stopped;
                    job.state_should_be_reported = true;
                }
                // no other results possible without specifying flags in waitpid
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    pub fn cleanup_terminated_jobs(&mut self) {
        self.jobs.retain(|j| !matches!(j.state, JobState::Done(_)));
        self.update_positions();
    }

    pub fn add_job(&mut self, pid: Pid, command: String, initial_state: JobState) {
        self.jobs.push(Job {
            position: JobPosition::Current,
            pid,
            command,
            state: initial_state,
            number: self.last_job_number,
            state_should_be_reported: initial_state != JobState::Running,
        });
        self.last_job_number += 1;
        self.update_positions();
    }

    fn job_index(&self, id: JobId) -> Option<usize> {
        match id {
            JobId::CurrentJob => self.jobs.last().map(|_| self.jobs.len() - 1),
            JobId::PreviousJob => {
                if self.jobs.len() > 1 {
                    Some(self.jobs.len() - 2)
                } else {
                    None
                }
            }
            JobId::JobNumber(n) => self.jobs.iter().position(|j| j.number == n),
            JobId::BeginsWith(s) => self.jobs.iter().position(|j| j.command.starts_with(s)),
            JobId::Contains(s) => self.jobs.iter().position(|j| j.command.contains(s)),
        }
    }

    pub fn get_job(&self, id: JobId) -> Option<&Job> {
        self.job_index(id).map(|i| &self.jobs[i])
    }

    pub fn get_job_mut(&mut self, id: JobId) -> Option<&mut Job> {
        self.job_index(id).map(|i| &mut self.jobs[i])
    }

    pub fn current(&self) -> Option<&Job> {
        self.jobs.last()
    }

    pub fn current_mut(&mut self) -> Option<&mut Job> {
        self.jobs.last_mut()
    }

    pub fn remove_job(&mut self, id: JobId) -> Option<Job> {
        if let Some(i) = self.job_index(id) {
            let job = self.jobs.remove(i);
            self.update_positions();
            Some(job)
        } else {
            None
        }
    }

    pub fn drain(&mut self) -> Vec<Job> {
        let mut jobs = Vec::new();
        std::mem::swap(&mut jobs, &mut self.jobs);
        jobs
    }

    pub fn iter(&self) -> impl Iterator<Item = &Job> {
        self.jobs.iter()
    }

    pub fn write_report<W: FnMut(&Job)>(&mut self, mut writer: W) {
        for job in self.jobs.iter_mut() {
            if job.state_should_be_reported {
                writer(job)
            }
            job.state_should_be_reported = false;
        }
    }
}

impl Default for JobManager {
    fn default() -> Self {
        Self {
            jobs: Vec::new(),
            last_job_number: 1,
        }
    }
}
