use nix::unistd::Pid;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum JobState {
    Current,
    Previous,
    Default,
    Terminated,
}

#[derive(Clone)]
pub struct Job {
    pub command: String,
    pub pid: Pid,
    pub number: u64,
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
