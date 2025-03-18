use nix::unistd::Pid;

#[derive(Clone)]
pub struct Job {
    pub command: String,
    pub pid: Pid,
    pub number: u64,
    pub terminated: bool,
}

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
        "%?" => {
            if text.len() < 4 {
                Err(())
            } else {
                Ok(JobId::Contains(&text[3..]))
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
