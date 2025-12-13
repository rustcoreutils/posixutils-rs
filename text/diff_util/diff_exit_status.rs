use std::process::Termination;

use super::constants::{EXIT_STATUS_DIFFERENCE, EXIT_STATUS_NO_DIFFERENCE, EXIT_STATUS_TROUBLE};

#[derive(Clone, Copy)]
pub enum DiffExitStatus {
    NotDifferent,
    Different,
    Trouble,
}

impl DiffExitStatus {
    pub fn status_code(&self) -> u8 {
        match self {
            DiffExitStatus::NotDifferent => EXIT_STATUS_NO_DIFFERENCE,
            DiffExitStatus::Different => EXIT_STATUS_DIFFERENCE,
            DiffExitStatus::Trouble => EXIT_STATUS_TROUBLE,
        }
    }
}

impl Termination for DiffExitStatus {
    fn report(self) -> std::process::ExitCode {
        std::process::ExitCode::from(self.status_code())
    }
}
