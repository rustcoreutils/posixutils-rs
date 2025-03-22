use crate::utils::{waitpid, OsResult};
use nix::sys::wait::{WaitPidFlag, WaitStatus};
use nix::unistd::Pid;
use std::slice::Iter;
use std::vec::Drain;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum JobPosition {
    Current,
    Previous,
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum JobState {
    Done(i32),
    Running,
    Stopped,
}

#[derive(Clone)]
pub struct Job {
    pub command: String,
    pub pid: Pid,
    pub number: u64,
    pub position: JobPosition,
    pub state: JobState,
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
        self.jobs
            .last_mut()
            .map(|j| j.position = JobPosition::Current);
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
                }
                WaitStatus::Signaled(_, _, _) => todo!(),
                WaitStatus::StillAlive => {}
                WaitStatus::Stopped(_, _) => {
                    job.state = JobState::Stopped;
                }
                _ => {
                    todo!()
                }
            }
        }
        Ok(())
    }

    pub fn cleanup_terminated_jobs(&mut self) {
        self.jobs.retain(|j| match j.state {
            JobState::Done(_) => false,
            _ => true,
        });
        self.update_positions();
    }

    pub fn add_job(&mut self, pid: Pid, command: String) {
        self.jobs.push(Job {
            position: JobPosition::Current,
            pid,
            command,
            state: JobState::Running,
            number: self.last_job_number,
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

    pub fn current(&self) -> Option<&Job> {
        self.jobs.last()
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
}

impl Default for JobManager {
    fn default() -> Self {
        Self {
            jobs: Vec::new(),
            last_job_number: 1,
        }
    }
}
