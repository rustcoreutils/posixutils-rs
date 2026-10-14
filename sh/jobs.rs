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
use crate::os::{waitpid, OsResult, Pid, TtyModes, WaitOptions, WaitStatus};
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum JobState {
    Done(libc::c_int),
    /// Terminated by a signal (must display distinctly and name the signal).
    Signaled(TermSignal),
    Running,
    /// Suspended, by the signal given.
    Stopped(TermSignal),
}

impl JobState {
    pub fn is_terminated(self) -> bool {
        matches!(self, JobState::Done(_) | JobState::Signaled(_))
    }

    /// The status `$?`, `wait` and `fg` report for a job in this state.
    pub fn exit_status(self) -> libc::c_int {
        match self {
            JobState::Done(code) => code,
            JobState::Signaled(signal) | JobState::Stopped(signal) => signal.exit_status(),
            JobState::Running => 0,
        }
    }
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
            // XCU jobs: plain "Stopped" is a job suspended by SIGTSTP; any
            // other stop signal is named.
            JobState::Stopped(signal) if signal.is(Signal::SigTstp) => f.write_str("Stopped"),
            JobState::Stopped(signal) => write!(f, "Stopped (SIG{signal})"),
        }
    }
}

/// What a single process of a job is doing, as `waitpid` last reported it.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ProcessState {
    Running,
    Stopped(TermSignal),
    Exited(libc::c_int),
    Signaled(TermSignal),
}

impl ProcessState {
    fn is_terminated(self) -> bool {
        matches!(self, ProcessState::Exited(_) | ProcessState::Signaled(_))
    }

    /// The status this process contributes to a terminated job.
    fn termination(self) -> JobState {
        match self {
            ProcessState::Signaled(signal) => JobState::Signaled(signal),
            ProcessState::Exited(code) => JobState::Done(code),
            // not reached for a terminated job
            ProcessState::Running | ProcessState::Stopped(_) => JobState::Done(0),
        }
    }
}

#[derive(Clone)]
pub struct JobProcess {
    pub pid: Pid,
    pub state: ProcessState,
}

#[derive(Clone)]
pub struct Job {
    pub command: String,
    /// The process group of the job's processes, when the shell made one for
    /// it (under job control); `None` when they are in the shell's own.
    pub process_group: Option<Pid>,
    /// Every process the shell created for the job, in pipeline order.
    pub processes: Vec<JobProcess>,
    /// `set -o pipefail` was on when the job started, so its status is that
    /// of the rightmost process that failed rather than the last one's.
    pub pipefail: bool,
    /// The terminal's modes when the job last stopped in the foreground,
    /// which `fg` gives back to it (an interactive shell only).
    pub tty_modes: Option<TtyModes>,
    pub number: u64,
    pub position: JobPosition,
    pub state_should_be_reported: bool,
}

impl Job {
    /// A running job of the processes `pids`, not yet in a job table.
    pub fn new(pids: &[Pid], process_group: Option<Pid>, command: String, pipefail: bool) -> Self {
        Self {
            command,
            process_group,
            processes: pids
                .iter()
                .map(|&pid| JobProcess {
                    pid,
                    state: ProcessState::Running,
                })
                .collect(),
            pipefail,
            tty_modes: None,
            number: 0,
            position: JobPosition::Other,
            state_should_be_reported: false,
        }
    }

    /// The process ID known for the job (XCU 2.11): its process group
    /// leader, else its first process.
    pub fn pid(&self) -> Pid {
        self.process_group
            .or_else(|| self.processes.first().map(|process| process.pid))
            .unwrap_or(0)
    }

    /// What `kill` signals for the job: its whole process group when it has
    /// one of its own, else its process.
    pub fn signal_target(&self) -> Pid {
        match self.process_group {
            Some(pgid) => -pgid,
            None => self.pid(),
        }
    }

    pub fn contains(&self, pid: Pid) -> bool {
        self.processes.iter().any(|process| process.pid == pid)
    }

    /// The job is stopped if any of its processes is, running while any
    /// process runs, and has terminated once all have: with the status of
    /// the last process, or under pipefail of the rightmost one that failed.
    pub fn state(&self) -> JobState {
        if let Some(stopped) = self
            .processes
            .iter()
            .find_map(|process| match process.state {
                ProcessState::Stopped(signal) => Some(signal),
                _ => None,
            })
        {
            return JobState::Stopped(stopped);
        }
        if !self.is_terminated() {
            return JobState::Running;
        }
        let last = self
            .processes
            .last()
            .map(|process| process.state.termination())
            .unwrap_or(JobState::Done(0));
        if self.pipefail {
            self.processes
                .iter()
                .rev()
                .map(|process| process.state.termination())
                .find(|state| state.exit_status() != 0)
                .unwrap_or(last)
        } else {
            last
        }
    }

    pub fn is_terminated(&self) -> bool {
        self.processes
            .iter()
            .all(|process| process.state.is_terminated())
    }

    /// The processes that have not terminated yet.
    pub fn live_pids(&self) -> Vec<Pid> {
        self.processes
            .iter()
            .filter(|process| !process.state.is_terminated())
            .map(|process| process.pid)
            .collect()
    }

    /// Records what `waitpid` reported for `pid`. Returns false when `pid` is
    /// not one of the job's processes. A job that has just stopped or
    /// terminated is to be reported.
    pub fn record(&mut self, pid: Pid, status: &WaitStatus) -> bool {
        let Some(index) = self.processes.iter().position(|process| process.pid == pid) else {
            return false;
        };
        let new_state = match status {
            WaitStatus::Exited { exit_status } => ProcessState::Exited(*exit_status),
            WaitStatus::Signaled { signal } => ProcessState::Signaled(*signal),
            WaitStatus::Stopped { signal } => ProcessState::Stopped(*signal),
            WaitStatus::Continued => ProcessState::Running,
            WaitStatus::StillAlive | WaitStatus::Interrupted => return true,
        };
        self.set_process_state(index, new_state);
        true
    }

    /// Records that `pid` has gone without a status left to read: it was
    /// never this process's child, or something else reaped it.
    pub fn record_lost(&mut self, pid: Pid, exit_status: libc::c_int) {
        if let Some(process) = self.processes.iter_mut().find(|process| process.pid == pid) {
            process.state = ProcessState::Exited(exit_status);
        }
    }

    fn set_process_state(&mut self, index: usize, new_state: ProcessState) {
        let before = self.state();
        self.processes[index].state = new_state;
        let after = self.state();
        if after != before && after != JobState::Running {
            self.state_should_be_reported = true;
        }
    }

    /// Marks every stopped process as running again, once SIGCONT has been
    /// sent to them.
    pub fn mark_continued(&mut self) {
        for process in &mut self.processes {
            if matches!(process.state, ProcessState::Stopped(_)) {
                process.state = ProcessState::Running;
            }
        }
    }

    /// `jobs -l`: the POSIX format with the process (group) id before the
    /// state.
    pub fn to_string_long(&self) -> String {
        format!(
            "[{}] {} {} {} {}\n",
            self.number,
            self.position,
            self.pid(),
            self.state(),
            self.command
        )
    }

    /// The POSIX `jobs` format: `"[%d] %c %s %s\n"`.
    pub fn to_string_short(&self) -> String {
        format!(
            "[{}] {} {} {}\n",
            self.number,
            self.position,
            self.state(),
            self.command
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

#[derive(Clone, Default)]
pub struct JobManager {
    jobs: Vec<Job>,
    /// Statuses of jobs `wait` has already collected, by process id. A
    /// collected job leaves the table -- `jobs` must not list it -- but
    /// `wait` on one of its processes keeps reporting its status, which is
    /// what dash and bash do.
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

    /// Polls every process of every job that has not terminated, recording
    /// each one that has stopped, been continued or terminated.
    pub fn update_jobs(&mut self) -> OsResult<()> {
        for job in &mut self.jobs {
            for pid in job.live_pids() {
                match waitpid(pid, WaitOptions::poll_all()) {
                    Ok(status) => {
                        job.record(pid, &status);
                    }
                    Err(err) if err.errno == Errno::ECHILD => {
                        // Already reaped — `wait` collects a job directly, and
                        // the shell may also be running without job control.
                        // There is no status left to read, and nothing to
                        // report.
                        job.record_lost(pid, 0);
                    }
                    Err(err) => return Err(err),
                }
            }
        }
        Ok(())
    }

    /// Records what `waitpid` reported for `pid` in the job it belongs to.
    /// Returns false when no job has that process.
    pub fn record_status(&mut self, pid: Pid, status: &WaitStatus) -> bool {
        self.jobs.iter_mut().any(|job| job.record(pid, status))
    }

    /// Records that `pid`, a process of a job, is gone with no status left.
    pub fn record_lost(&mut self, pid: Pid, exit_status: libc::c_int) {
        for job in &mut self.jobs {
            job.record_lost(pid, exit_status);
        }
    }

    /// The job that has `pid` among its processes.
    pub fn job_with_process(&self, pid: Pid) -> Option<&Job> {
        self.jobs.iter().find(|job| job.contains(pid))
    }

    /// The status of a job that has already terminated, taken from the job
    /// table or from the memory of jobs already collected. `wait` asks this
    /// before calling `waitpid`, which would fail with ECHILD once the periodic
    /// sweep has reaped the child.
    pub fn take_collected_status(&mut self, pid: Pid) -> Option<i32> {
        if let Some(job) = self.job_with_process(pid) {
            let state = job.state();
            if !state.is_terminated() {
                // still alive, so the caller has to wait for it after all
                return None;
            }
            let status = state.exit_status();
            self.collect(pid, status);
            return Some(status);
        }
        self.collected
            .iter()
            .find(|(collected, _)| *collected == pid)
            .map(|(_, status)| *status)
    }

    /// Records the status of a terminated job, the one with process `pid`,
    /// and drops it from the table.
    pub fn collect(&mut self, pid: Pid, status: i32) {
        let mut pids = vec![pid];
        if let Some(index) = self.jobs.iter().position(|job| job.contains(pid)) {
            let job = self.jobs.remove(index);
            pids = job.processes.iter().map(|process| process.pid).collect();
            self.update_positions();
        }
        for pid in pids {
            self.remember(pid, status);
        }
    }

    /// Moves every terminated job out of the table, keeping its status. Nothing
    /// bounds how many background commands a loop starts, so the table cannot
    /// be allowed to keep one entry per iteration.
    pub fn collect_terminated_jobs(&mut self) {
        let mut terminated = Vec::new();
        self.jobs.retain(|job| {
            let state = job.state();
            if !state.is_terminated() {
                return true;
            }
            for process in &job.processes {
                terminated.push((process.pid, state.exit_status()));
            }
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

    /// Adds `job` to the table as the current job, under the lowest job
    /// number not in use, as in dash (bash also starts again from 1 once
    /// the table is empty).
    pub fn add_job(&mut self, mut job: Job) {
        job.number = (1..)
            .find(|number| self.jobs.iter().all(|job| job.number != *number))
            .unwrap_or(1);
        self.reinsert_job(job);
    }

    /// Puts back a job that `fg` took out of the table and that has stopped
    /// again: it keeps its job number, and becomes the current job.
    pub fn reinsert_job(&mut self, job: Job) {
        self.jobs.push(job);
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

    /// Drops every pending report: a subshell has the parent's job table to
    /// list, but the changes in it are the parent's to report.
    pub fn forget_reports(&mut self) {
        for job in &mut self.jobs {
            job.state_should_be_reported = false;
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn exited(code: i32) -> WaitStatus {
        WaitStatus::Exited { exit_status: code }
    }

    fn stopped(signal: Signal) -> WaitStatus {
        WaitStatus::Stopped {
            signal: TermSignal(signal.into()),
        }
    }

    #[test]
    fn a_job_is_stopped_while_any_process_is() {
        let mut job = Job::new(&[10, 11], Some(10), "a | b".to_string(), false);
        assert_eq!(job.state(), JobState::Running);
        job.record(11, &stopped(Signal::SigTstp));
        assert_eq!(job.state(), JobState::Stopped(TermSignal(libc::SIGTSTP)));
        assert!(job.state_should_be_reported);
        job.state_should_be_reported = false;
        job.record(11, &WaitStatus::Continued);
        assert_eq!(job.state(), JobState::Running);
        // being continued is not reported
        assert!(!job.state_should_be_reported);
    }

    #[test]
    fn a_job_terminates_with_its_last_process_status() {
        let mut job = Job::new(&[10, 11], Some(10), "a | b".to_string(), false);
        job.record(11, &exited(3));
        assert_eq!(job.state(), JobState::Running);
        job.record(
            10,
            &WaitStatus::Signaled {
                signal: TermSignal(libc::SIGPIPE),
            },
        );
        assert_eq!(job.state(), JobState::Done(3));
        assert!(job.state_should_be_reported);
    }

    #[test]
    fn pipefail_takes_the_rightmost_failure() {
        let mut job = Job::new(&[10, 11, 12], Some(10), "a | b | c".to_string(), true);
        job.record(10, &exited(1));
        job.record(11, &exited(2));
        job.record(12, &exited(0));
        assert_eq!(job.state(), JobState::Done(2));
    }

    #[test]
    fn a_job_put_back_keeps_its_number() {
        let mut jobs = JobManager::default();
        jobs.add_job(Job::new(&[10], Some(10), "a".to_string(), false));
        jobs.add_job(Job::new(&[20], Some(20), "b".to_string(), false));
        let job = jobs.remove_job(JobId::JobNumber(1)).unwrap();
        jobs.reinsert_job(job);
        let current = jobs.get_job(JobId::CurrentJob).unwrap();
        assert_eq!(current.number, 1);
        assert_eq!(current.command, "a");
    }

    #[test]
    fn stopped_by_tstp_is_plain_stopped() {
        assert_eq!(
            JobState::Stopped(TermSignal(libc::SIGTSTP)).to_string(),
            "Stopped"
        );
        assert_eq!(
            JobState::Stopped(TermSignal(libc::SIGSTOP)).to_string(),
            "Stopped (SIGSTOP)"
        );
    }
}
