//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::os::errno::Errno;
use crate::os::signals::{Signal, TermSignal};
use crate::os::{waitpid, OsResult, Pid, WaitStatus};
use std::collections::VecDeque;
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
    Done(libc::c_int),
    /// Terminated by a signal (must display distinctly and name the signal).
    Signaled(TermSignal),
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
            JobState::Signaled(signal) => write!(f, "Terminated (SIG{signal})"),
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
    /// `jobs -l`: the POSIX format with the process id before the state.
    pub fn to_string_long(&self) -> String {
        format!(
            "[{}] {} {} {} {}\n",
            self.number, self.position, self.pid, self.state, self.command
        )
    }

    /// The POSIX `jobs` format: `"[%d] %c %s %s\n"`.
    pub fn to_string_short(&self) -> String {
        format!(
            "[{}] {} {} {}\n",
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

/// Parses a job identifier. Returns `Err` if `text` is not a valid job id
/// (including the case where it does not begin with '%').
pub fn parse_job_id(text: &str) -> Result<JobId<'_>, ()> {
    if !text.starts_with('%') {
        return Err(());
    }
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

/// How many collected statuses to keep. Nothing bounds how many background
/// commands a script starts, and `wait` on a pid this far back is vanishingly
/// rare, so the memory is a ring rather than a table that grows forever.
const COLLECTED_MEMORY: usize = 32;

#[derive(Clone)]
pub struct JobManager {
    jobs: Vec<Job>,
    last_job_number: u64,
    /// Statuses of jobs `wait` has already collected. A collected job leaves
    /// the table -- `jobs` must not list it -- but `wait` on the same pid keeps
    /// reporting its status, which is what dash and bash do.
    collected: VecDeque<(Pid, i32)>,
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
            if matches!(job.state, JobState::Done(_) | JobState::Signaled(_)) {
                continue;
            }
            let status = match waitpid(job.pid, true, true) {
                Ok(status) => status,
                Err(err) if err.errno == Errno::ECHILD => {
                    // Already reaped — `wait` collects a job directly, and the
                    // shell may also be running without job control. There is
                    // no status left to read, and nothing to report.
                    job.state = JobState::Done(0);
                    continue;
                }
                Err(err) => return Err(err),
            };
            match status {
                WaitStatus::Exited { exit_status } => {
                    job.state = JobState::Done(exit_status);
                    job.state_should_be_reported = true;
                }
                WaitStatus::Signaled { signal, .. } => {
                    if signal.is(Signal::SigStop) {
                        job.state = JobState::Stopped;
                    } else {
                        job.state = JobState::Signaled(signal);
                    }
                    job.state_should_be_reported = true;
                }
                // EINTR while polling a background job: nothing to record.
                WaitStatus::StillAlive | WaitStatus::Interrupted => {}
                WaitStatus::Stopped { .. } => {
                    job.state = JobState::Stopped;
                    job.state_should_be_reported = true;
                }
            }
        }
        Ok(())
    }

    /// The status of a job that has already terminated, taken from the job
    /// table or from the memory of jobs already collected. `wait` asks this
    /// before calling `waitpid`, which would fail with ECHILD once the periodic
    /// sweep has reaped the child.
    pub fn take_collected_status(&mut self, pid: Pid) -> Option<i32> {
        if let Some(pos) = self.jobs.iter().position(|job| job.pid == pid) {
            let status = match self.jobs[pos].state {
                JobState::Done(status) => status,
                JobState::Signaled(signal) => signal.exit_status(),
                // still alive, so the caller has to wait for it after all
                JobState::Running | JobState::Stopped => return None,
            };
            self.collect(pid, status);
            return Some(status);
        }
        self.collected
            .iter()
            .find(|(collected, _)| *collected == pid)
            .map(|(_, status)| *status)
    }

    /// Records the status of a terminated job and drops it from the table.
    pub fn collect(&mut self, pid: Pid, status: i32) {
        self.jobs.retain(|job| job.pid != pid);
        self.update_positions();
        self.remember(pid, status);
    }

    /// Moves every terminated job out of the table, keeping its status. Nothing
    /// bounds how many background commands a loop starts, so the table cannot
    /// be allowed to keep one entry per iteration.
    pub fn collect_terminated_jobs(&mut self) {
        let mut terminated = Vec::new();
        self.jobs.retain(|job| {
            let status = match job.state {
                JobState::Done(status) => status,
                JobState::Signaled(signal) => signal.exit_status(),
                JobState::Running | JobState::Stopped => return true,
            };
            terminated.push((job.pid, status));
            false
        });
        if terminated.is_empty() {
            return;
        }
        self.update_positions();
        for (pid, status) in terminated {
            self.remember(pid, status);
        }
    }

    fn remember(&mut self, pid: Pid, status: i32) {
        self.collected.retain(|(collected, _)| *collected != pid);
        if self.collected.len() == COLLECTED_MEMORY {
            self.collected.pop_front();
        }
        self.collected.push_back((pid, status));
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

    pub fn current_mut(&mut self) -> Option<&mut Job> {
        self.jobs.last_mut()
    }

    /// Marks an already-known job as stopped. Returns false when `pid` is not
    /// in the table, so the caller can register it instead of duplicating it.
    pub fn mark_stopped_by_pid(&mut self, pid: Pid) -> bool {
        if let Some(job) = self.jobs.iter_mut().find(|job| job.pid == pid) {
            job.state = JobState::Stopped;
            job.state_should_be_reported = true;
            true
        } else {
            false
        }
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
            collected: VecDeque::new(),
        }
    }
}
